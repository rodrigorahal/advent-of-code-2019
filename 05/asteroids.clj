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

(defn update-state [state opcode params modes]
    (cond (jump? opcode) state
          (= opcode 3) (assoc state 
                              (first params) 
                              (do (print "Choose \"1\" for Part 1 or \"5\" for Part 2: ") (flush) (read-string (read-line))))
          (= opcode 4) (do (println (parameter-value state (first params) (first modes))) state)
          :else (let [[i j target] params
                      [mi mj mt] modes]
                        (assoc state 
                               target
                               ((opcode-to-op opcode) 
                                    (parameter-value state i mi)
                                    (parameter-value state j mj))))))

(defn run-intcode [initial-state]
  (loop [state initial-state
         pos 0]
        (let [instruction (state pos)
              opcode (instruction->opcode instruction)]
          (if (halt? opcode) ;halt
              state
              (let [params (instruction->parameters opcode state pos)
                    modes (instruction->modes instruction)]
                (recur (update-state state opcode params modes)
                       (update-pointer pos state opcode params modes)))))))

; examples:
; state [1002 4 3 4 33]
; inst 1002
; parameters [4 3 4]
; modes (0 1 0)
(defn read-input []
  (vec (map read-string (str/split (slurp "input.txt") #","))))

(def initial-state (read-input))

(run-intcode initial-state)

