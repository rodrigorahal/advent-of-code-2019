(require '[clojure.string :as str])

(def opcode-to-op 
  {1 + 
   2 *
  })

(defn update-state [state instruction]
  (let [[opcode i j target] instruction]
    (assoc state target ((opcode-to-op opcode) (state i) (state j)))))

(defn run-intcode [initial-state]
  (loop [state initial-state
         pos 0]
        (let [instruction (subvec state pos (+ pos 4))]
          (if (= (first instruction) 99) ;halt
              state
              (recur (update-state state instruction)
                     (+ pos 4))))))

(defn read-input []
  (vec (map read-string (str/split (slurp "input.txt") #","))))

(def input (read-input))

(defn initial-state [noun verb]
    (assoc (assoc input 1 noun) 2 verb))

(assert (= (first (run-intcode [1,9,10,3,2,3,11,0,99,30,40,50])) 3500))

(println "Answer: "
  (first (run-intcode (initial-state 9 20))))

(def initial-states
  (for [x (range 100) y (range 100)]
    {:noun x
     :verb y 
     :initial-state (initial-state x y)
    }))

(def final-states
  (map (fn [m] (assoc m :final-state (run-intcode (m :initial-state))))
       initial-states))

(defn find-target [states target]
  (select-keys 
    (first 
      (filter 
        (fn [m] (= (first (m :final-state)) target)) 
        states)) [:noun :verb]))

(println "Answer: " 
  (apply (fn [noun verb] (+ (* 100 noun) verb))
         (vals (find-target final-states 19690720))))


