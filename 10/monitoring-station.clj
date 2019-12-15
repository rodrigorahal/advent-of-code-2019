(require '[clojure.string :as str])

(defn rotate [angle by]
    (let [n (+ angle by) two-pi (* 2 Math/PI)]
        (if (>= n two-pi)
            (- n two-pi)
            n)))

(defn manhattan-distance [a b]
    (let [[x1 y1] a
         [x2 y2] b]
        (+
            (Math/abs (- x2 x1))
            (Math/abs (- y2 y1)))))

(defn angle [a b]
    (let [[xa ya] a
          [xb yb] b
           dx (- xb xa)
           dy (- yb ya)
           a (Math/atan2 dy dx)]
           (if (>= a 0)
               (rotate a (* 1/2 (Math/PI)))
               (rotate (+ (* (Math/PI) 2) a) (* 1/2 (Math/PI))))))


(defn angles [asteroids]
    (let [angs (for [a asteroids
                      b asteroids
              :when (not= a b)]
            [a (angle a b)])]
        (reduce
            (fn [m [a g]] (assoc m a (conj (m a) g)))
             {}
            angs)))

(defn most-visible [asteroids]
    (let [angs (angles asteroids)
          visible (into {} (for [[k v] angs] [k (count (set v))]))]
        (apply max-key val visible)))

(defn sort-grouped-polars [grouped-polars]
    (into (sorted-map)
        (for [[a ps] grouped-polars]
            [a (sort-by #(% :dist) ps)])))


(defn group-polars [polars]
    (group-by :angle polars))

(defn polars-by-asteroid [asteroids]
    (let [angs (for [a asteroids
                      b asteroids
              :when (not= a b)]
            [a {:to b :angle (angle a b) :dist (manhattan-distance a b)}])]
        (reduce
            (fn [m [a g]] (assoc m a (conj (m a) g)))
             {}
            angs)))

(defn run-vaporize [asteroids base n]
    (let [sorted-grouped-polars
            (sort-grouped-polars
                (group-polars
                    ((polars-by-asteroid asteroids) base)))]
    (loop
        [polars sorted-grouped-polars
            angles (into [] (keys polars))
            vaporized []]
        (if (= (count vaporized) n)
            (last vaporized)
            (let [angle (first angles)
                    ps (polars angle)
                    next (first ps)]
                (recur
                    (assoc polars angle (rest ps))
                    (if (empty? (rest ps))
                                (rest angles)
                                (conj (into [] (rest angles)) angle))
                    (conj vaporized next)))))))

(defn read-input [filename]
    (with-open [rdr (clojure.java.io/reader filename)]
        (let [rows (line-seq rdr)]
            (loop [row nil
                   to-process rows
                   y 0
                   coords []]
                   (if (empty? to-process)
                       coords
                       (recur
                            (first to-process)
                            (rest to-process)
                            (inc y)
                            (concat coords
                                    (keep-indexed #(if (= %2 "#") [%1 y])
                                                  (str/split (first to-process) #"")))))))))

(def asteroids (read-input "input.txt"))

(println "Answer Part 1: " (second (most-visible asteroids)))

(println "Answer Part 2: " ((run-vaporize asteroids [20 19] 200) :to))