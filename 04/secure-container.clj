(defn equal-adjacents? [password]
    (some #(= (first %) (second %)) (partition 2 1 (str password))))


(defn stronger-equal-adjacents? [password]
  (let [counts (frequencies (str password))]
    (some (fn [[a b]] (and (= a b) (= 2 (counts a))))
          (partition 2 1 (str password)))))        

(defn digits [n]
  (->> n str (map (comp read-string str))))

(defn non-decreasing? [password]
    (apply <= (digits password)))

(defn valid? [password]
    (and 
        (equal-adjacents? password)
        (non-decreasing? password)))

(defn stronger-valid? [password]
    (and (valid? password)
         (stronger-equal-adjacents? password)))



(defn valid-passwords [start end]
    (for [password (range start end)
          :when (valid? password)]

    password))

(println "Answer : "
    (count (valid-passwords 183564 657474)))

(defn stronger-valid-passwords [start end]
    (for [password (range start end)
          :when (stronger-valid? password)]

    password))

(println "Answer : "
    (count (stronger-valid-passwords 183564 657474)))