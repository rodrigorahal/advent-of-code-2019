(require '[clojure.string :as str])

(defn manhattan-distance [a b]
    (let [[x1 y1] a
         [x2 y2] b]
        (+
            (Math/abs (- x2 x1))
            (Math/abs (- y2 y1)))))

; lines: [{:direction "U" :points [[0 0] [0 8]] :distance 8}
;         {:direction "R" :points [[0 8] [4 8]] :distance 4}]
; points [[0 0] [0 8]]
(defn is-horizontal? [line]
    (contains? #{ "R" "L"} (line :direction)))
(defn is-vertical? [line]
    (contains? #{ "U" "D"} (line :direction)))

(defn horizontal [lines]
    (first (filter is-horizontal? lines)))
(defn vertical [lines]
    (first (filter is-vertical? lines)))

(defn extremes [extreme-fn map-fn points]
    (->> points
        (map map-fn)
        (apply extreme-fn)))

(defn max-x [points]
    (extremes max first points))
(defn min-x [points]
    (extremes min first points))
(defn max-y [points]
    (extremes max second points))
(defn min-y [points]
    (extremes min second points))

(defn extremes [extreme-fn map-fn points]
    (->> points
        (map map-fn)
        (apply extreme-fn)))

(defn angle-intercept [hori vert]
    (let [h (hori :points)
          v (vert :points)
          x-v (ffirst v)
          y-h (second (first h))]
          (if 
            (and (<= (min-x h) x-v (max-x h))
                 (<= (min-y v) y-h (max-y v)))
            [x-v y-h]
            nil)))

(defn intercept [lines]
    (cond (every? is-horizontal? lines) nil 
          (every? is-vertical? lines) nil
          :else (let [hori (horizontal lines)
                      vert (vertical lines)]
                      (angle-intercept hori vert))))

;wires [[lines] [lines]]
(defn find-intercepts [wires]
     (rest(map vec
      (partition 2 
        (remove nil?
        (flatten
            (for [line-one (first wires)
                  line-two (second wires)]
                (intercept [line-one line-two]))))))))

(defn closest-intercept [intercepts]
    (->> intercepts
         (sort-by #(manhattan-distance [0 0] %))
         (first)))

; R8,U5,L5,D3
(def wire-one [
    {:direction "R" :points [[0 0] [8 0]]}
    {:direction "U" :points [[8 0] [8 5]]}
    {:direction "L" :points [[8 5] [3 5]]}
    {:direction "D" :points [[3 5] [3 2]]}
])

; U7,R6,D4,L4
(def wire-two [
    {:direction "U" :points [[0 0] [0 7]]}
    {:direction "R" :points [[0 7] [6 7]]}
    {:direction "D" :points [[6 7] [6 3]]}
    {:direction "L" :points [[6 3] [2 3]]}
])

(defn read-input [filename]
    (with-open [rdr (clojure.java.io/reader filename)]
        (map #(str/split % #",") (reduce conj [] (line-seq rdr)))))


(defn next-point [[x y] direction distance]
    (cond (= direction "U") [x (+ y distance)]
          (= direction "D") [x (- y distance)]
          (= direction "L") [(- x distance) y]
          (= direction "R") [(+ x distance) y]))

; input ["U7" "R6" "D4" "L4"]
(defn make-wire [input]
    (loop [prev-point [0 0]
           wire []
           to-process input]
           (if (empty? to-process)
                wire
                (let [next-code (first to-process)
                      direction (str (first next-code))
                      distance (read-string (apply str (rest next-code)))
                      point (next-point prev-point direction distance)]
                (recur 
                    point
                    (conj wire {:direction direction :points [prev-point point]})
                    (rest to-process))))))

(assert (= wire-two (make-wire ["U7" "R6" "D4" "L4"])))

(def wires (map make-wire (read-input "input.txt")))

(def closest (closest-intercept (find-intercepts wires)))
(println "Answer: " (apply + closest))

(defn point-in-line? [line point]
    (let [l (line :points)
          [xp yp] point]
          (and 
            (<= (min-x l) xp (max-x l))
            (<= (min-y l) yp (max-y l)))))

(defn steps-to-intercept [wire intercept]
    (loop [lines wire
           steps 0]
           (let [line (first lines)
                 start (first (line :points))]
            (if (point-in-line? line intercept)
                (+ steps (manhattan-distance start intercept))
                (recur 
                (rest lines)
                (+ steps (apply manhattan-distance (line :points))))))))

(defn intercepts-to-steps [wire intercepts]
    (map #(steps-to-intercept wire %) intercepts))

(defn steps [wires intercepts]
    (map 
        vector
        (intercepts-to-steps (first wires) intercepts)
        (intercepts-to-steps (second wires) intercepts)))

(defn min-steps [steps]
    (->> steps
         (map #(apply + %))
         (apply min)))

(println "Answer: " 
    (min-steps (steps wires (find-intercepts wires))))