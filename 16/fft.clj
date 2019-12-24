(def pattern [0 1 0 -1])

(defn digits [num]
  (->> (str num)
       (map str)
       (map read-string)))

(defn repeat-by [pattern i] 
    (apply concat (map #(repeat i %) pattern)))
    
(defn stretch [repeated-pattern digits]
    (if (< (dec (count repeated-pattern)) (count digits))
        (apply concat (repeat (inc (int (Math/ceil (/ (count digits) (count repeated-pattern))))) repeated-pattern))
        repeated-pattern))

(defn format-pattern [pattern digits i]
    (-> pattern
         (repeat-by i)
         (stretch digits)
         (rest)))

(defn patterns [pattern digits]
    (into {} 
         (for [i (range 1 (inc (count digits)))]
            [i (format-pattern pattern digits i)])))

(defn multiply [digits formatted-pattern]
    (->> (map vector digits formatted-pattern)
         (map #(apply * %))
         (reduce +)))

(defn last-digit [num]
    (Math/abs (rem num 10)))

(defn digit [digits pattern]
    (last-digit (multiply digits pattern)))

(defn step [digits patterns]
    (for [i (range 1 (inc (count digits)))]
        (digit digits (patterns i))))

(defn steps [digits patterns n]
    (loop [digits digits
           phase 0
           results []]
        (if (< phase n)
            (let [step' (step digits patterns)]
                (recur step'
                       (inc phase)
                       (conj results step')))
            results)))

(defn read-input []
    (read-string (slurp "16/input.txt")))

(def digits' (digits (read-input)))

(defn main []
    (def patterns' (patterns pattern digits'))
    (println "Part 1: " (clojure.string/join "" (take 8 (last (steps digits' patterns' 100))))))

(defn step-2 [digits]
    (let [partial-sum (reduce + digits)]
        (loop [curr-sum partial-sum
               i 0
               res []]

            (if (< i (count digits))
                (recur (- curr-sum (digits i))
                       (inc i)
                       (conj res (last-digit curr-sum)))
                res))))

(defn steps-2 [digits n]
    (loop [digits digits
           phase 0
           results []]
        (if (< phase n)
            (let [step' (step-2 digits)]
                (recur step'
                       (inc phase)
                       (conj results step')))
            results)))

(defn part2 []
    (def offset (read-string (clojure.string/join "" (take 7 digits'))))
    (def stupid-large-digits (apply concat (repeat 10000 digits')))
    (def digits' (vec (take-last (- (count stupid-large-digits) offset) stupid-large-digits)))
    (println "Part 2:" (clojure.string/join "" (take 8 (last (steps-2 digits' 100))))))

(main)
(part2)

