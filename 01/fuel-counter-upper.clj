(defn round-down [num]
    (-> num
        (Math/floor)
        (int)))

(defn fuel-required [mass]
    (-> mass
        (/ 3)
        (round-down)
        (- 2)))

(defn recursive-fuel-required [mass]
    (loop [fuels []
           curr-mass mass]
        (if (<= curr-mass 0)            
            (reduce + (filter #(> % 0)fuels))
            (recur 
                (conj fuels (fuel-required curr-mass))
                (fuel-required curr-mass)))))

(defn total-fuel-required [modules calculate-fuel]
    (reduce + (map calculate-fuel modules)))

(defn read-input []
    (with-open [rdr (clojure.java.io/reader "input.txt")]
        (map read-string (reduce conj [] (line-seq rdr)))))

(def modules (read-input))

(def answer-1 (total-fuel-required modules fuel-required))

(def answer-2 (total-fuel-required modules recursive-fuel-required))

(assert (= (fuel-required 12) 2))

(assert (= (fuel-required 14) 2))

(assert (= (fuel-required 1969) 654))

(assert (= (fuel-required 100756) 33583))

(assert (= (total-fuel-required [12 14 1969 100756] fuel-required) 34241))

(assert (= (recursive-fuel-required 14) 2))

(assert (= (recursive-fuel-required 1969) 966))

(assert (= (recursive-fuel-required 100756) 50346))

(println "Answer Part 1: " answer-1)
;=> 3125750

(println "Answer Part 2: " answer-2)
;=> 4685788