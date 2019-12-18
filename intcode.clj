(ns intcode
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout poll! go-loop]] 
            [clojure.string :as str]))

(def opcode-to-op 
  {1 (fn [a b] (+ a b))           ;sum
   2 (fn [a b] (* a b))           ;product
   7 (fn [a b] (if (< a b) 1 0))  ;less-than
   8 (fn [a b] (if (= a b) 1 0))  ;equals
  })

(def instruction-size-by-opcode {1 4, 2 4, 3 2, 4 2, 5 3, 6 3, 7 4, 8 4, 9 2})

(defn jump? [opcode] (contains? #{5 6} opcode))

(defn halt? [opcode] (= opcode 99))

(defn read-input? [opcode] (= opcode 3))

(defn write-output? [opcode] (= opcode 4))

(defn update-rel-base? [opcode] (= opcode 9))

(defn digits [num]
  (->> (str num)
       (map str)
       (map read-string)))

(defn instruction->opcode [inst]
    (cond (= (count (str inst)) 1) inst
          (= (count (str inst)) 2) inst
          :else (read-string (str (last (str inst))))))      

(defn instruction->modes [inst]
  (let [opcode (instruction->opcode inst)
        digs (digits inst)]
    (cond (= (count digs) 1) (repeat (dec (instruction-size-by-opcode opcode)) 0)
          :else (concat
                  (subvec (vec (reverse digs)) 2)
                  (repeat (- (dec (instruction-size-by-opcode opcode)) (- (count digs) 2)) 0)))))

(defn instruction->parameters [opcode state pos]
    (subvec state
            (inc pos)
            (+ pos (instruction-size-by-opcode opcode))))

(defn parameter-value [state param mode rel-base]
    (cond (= mode 0) (state param)
          (= mode 1) param
          (= mode 2) (state (+ rel-base param))
          :else (println "Unsupported mode ", mode)))

(defn parameter-address [param mode rel-base]
    (cond 
        (= mode 2) (+ rel-base param)
        :else param))

(defn update-rel-base [rel-base state opcode params modes]
    (cond 
        (= opcode 9) (+ rel-base (parameter-value state (first params) (first modes) rel-base))
        :else rel-base))

(defn update-pointer [pos state opcode params modes rel-base]
  (cond 
    (= opcode 5) 
      (if (not (zero? (parameter-value state (first params) (first modes) rel-base))) 
          (parameter-value state (second params) (second modes) rel-base)
          (+ pos 3))
    (= opcode 6)
      (if (zero? (parameter-value state (first params) (first modes) rel-base)) 
          (parameter-value state (second params) (second modes) rel-base)
          (+ pos 3))
    :else (->> opcode
               (instruction-size-by-opcode)
               (+ pos))))

(defn update-state [state opcode params modes rel-base in-chan out-chan]
    (cond 
        (jump? opcode) state
        (update-rel-base? opcode) state
        :else (let [[i j target] params
                    [mi mj mt] modes]
                        (assoc state 
                               (parameter-address target mt rel-base)
                               ((opcode-to-op opcode) 
                                    (parameter-value state i mi rel-base)
                                    (parameter-value state j mj rel-base))))))


;; hack 101
(defn allocate-additional-mem [initial-state size]
    (into [] (concat initial-state (repeat size 0))))

(defn run-intcode [initial-state & {:keys [add-mem-size] :or {add-mem-size 10}}]
  (let [in-chan (chan)
        out-chan (chan)]
    (go-loop [state (allocate-additional-mem initial-state (* add-mem-size (count initial-state)))
          pos 0
          rel-base 0]
          (let [instruction (state pos)
                opcode (instruction->opcode instruction)]
            (if (halt? opcode)
                (do (close! in-chan)       ;; close the channel
                    (poll! in-chan)        ;; drain the channel to unblock writers
                    (>!! out-chan opcode)) ;; communicate termination
                (let [params (instruction->parameters opcode state pos)
                      modes (instruction->modes instruction)
                      new-pointer (update-pointer pos state opcode params modes rel-base)
                      new-rel-base (update-rel-base rel-base state opcode params modes)]
                  (cond (read-input? opcode)
                        (let [read (<! in-chan)
                              new-state (assoc state 
                                              (parameter-address (first params) (first modes) rel-base)
                                              read)]
                          (recur new-state new-pointer new-rel-base))

                        (write-output? opcode)
                        (let [output (parameter-value state (first params) (first modes) rel-base)]
                              (>! out-chan output)
                              (recur state new-pointer new-rel-base))

                        :else 
                        (recur (update-state state opcode params modes rel-base in-chan out-chan)
                                new-pointer
                                new-rel-base))))))
  [in-chan out-chan]))

; examples:
; state [1002 4 3 4 33]
; inst 1002
; parameters [4 3 4]
; modes (0 1 0)
(defn read-input [filename]
  (vec (map read-string (str/split (slurp filename) #","))))