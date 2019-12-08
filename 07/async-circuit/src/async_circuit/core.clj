(ns async-circuit.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

(require '[clojure.string :as str])

(def opcode-to-op 
  {1 (fn [a b] (+ a b))           ;sum
   2 (fn [a b] (* a b))           ;product
   7 (fn [a b] (if (< a b) 1 0))  ;less-than
   8 (fn [a b] (if (= a b) 1 0))  ;equals
  })

(def instruction-size-by-opcode {1 4, 2 4, 3 2, 4 2, 5 3, 6 3, 7 4, 8 4})

(defn jump? [opcode] (contains? #{5 6} opcode))

(defn halt? [opcode] (= opcode 99))

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

(defn parameter-value [state param mode]
    (cond (= mode 0) (state param)
          (= mode 1) param
          :else (println "Unsupported mode ", mode)))

(defn update-pointer [pos state opcode params modes]
  (cond 
    (= opcode 5) 
      (if (not (zero? (parameter-value state (first params) (first modes)))) 
          (parameter-value state (second params) (second modes))
          (+ pos 3))
    (= opcode 6)
      (if (zero? (parameter-value state (first params) (first modes))) 
          (parameter-value state (second params) (second modes))
          (+ pos 3))
    :else (->> opcode
               (instruction-size-by-opcode)
               (+ pos))))

(defn update-state [state opcode params modes in-chan out-chans]
    (cond (jump? opcode) state
        (= opcode 3) (let [read (<!! in-chan)]
                        (assoc state 
                               (first params) 
                               read))
        (= opcode 4) (let [output (parameter-value state (first params) (first modes))]
                            (if (not (clojure.core.async.impl.protocols/closed? (first out-chans)))
                                (>!! (first out-chans) output)
                                (>!! (second out-chans) output))
                            state)
        :else (let [[i j target] params
                    [mi mj mt] modes]
                      (assoc state 
                              target
                              ((opcode-to-op opcode) 
                                  (parameter-value state i mi)
                                  (parameter-value state j mj))))))

(defn run-intcode [initial-state name in-chan out-chans]
  (loop [state initial-state
         pos 0]
        (let [instruction (state pos)
              opcode (instruction->opcode instruction)]
          (if (halt? opcode) ;halt
              (close! in-chan)
              (let [params (instruction->parameters opcode state pos)
                    modes (instruction->modes instruction)]
                (recur (update-state state opcode params modes in-chan out-chans)
                       (update-pointer pos state opcode params modes)))))))

; examples:
; state [1002 4 3 4 33]
; inst 1002
; parameters [4 3 4]
; modes (0 1 0)
(defn read-input [filename]
  (vec (map read-string (str/split (slurp filename) #","))))

(defn make-channels []
  {
    "A" (chan)
    "B" (chan)
    "C" (chan)
    "D" (chan)
    "E" (chan)
    "T" (chan)
  })

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn allowed-phase-settings [phases] (permutations phases))

(defn run-async-amplification [initial-state phases]
  (def channels (make-channels))
  (go (run-intcode initial-state "A" (channels "A") [(channels "B")]))
  (go (run-intcode initial-state "B" (channels "B") [(channels "C")]))
  (go (run-intcode initial-state "C" (channels "C") [(channels "D")]))
  (go (run-intcode initial-state "D" (channels "D") [(channels "E")]))
  (go (run-intcode initial-state "E" (channels "E") [(channels "A") (channels "T")]))

  (let [[a b c d e] phases]
    (>!! (channels "A") a)
    (>!! (channels "B") b)
    (>!! (channels "C") c)
    (>!! (channels "D") d)
    (>!! (channels "E") e)
    (>!! (channels "A") 0))

    (<!! (channels "T")))

(defn -main
  "Will run amplification circuit with channels."
  [& args]
  (def initial-state (read-input "resources/input.txt"))
  (println "Part 1: " 
    (apply max 
          (map #(run-async-amplification initial-state %) 
               (allowed-phase-settings [0 1 2 3 4]))))
  (println "Part 2: " 
    (apply max 
           (map #(run-async-amplification initial-state %) 
                (allowed-phase-settings [5 6 7 8 9]))))
)
