(require '[clojure.string :as str])

(defn read-input []
    (map read-string
        (-> "input.txt"
            (slurp)
            (str/split #""))))

(def pixels (read-input))

(def layers (map vec (partition (* 25 6) pixels)))

(defn count-num [layer num]
    (count (filter #(= % num) layer)))

(defn count-zeros [layer]
    (count-num layer 0))

(defn least-zeros [layers]
    (first (sort-by count-zeros layers)))

(println "Asnwer: "
    (let [lest-zeros-layer (least-zeros layers)]
        (*  (count-num lest-zeros-layer 1)
            (count-num lest-zeros-layer 2))))

(defn stack-layers [layers]
    (loop [pixels-by-position []
           position 0]
        (if (= position (* 25 6))
            pixels-by-position
            (recur 
                 (conj pixels-by-position (map #(% position) layers))
                 (inc position)))))

(defn visible-pixel [pixels]
    (loop [pixel (first pixels)
           behind (rest pixels)]
        (if (not (= pixel 2))
            pixel
            (recur (first behind)
                   (rest behind)))))

(defn visible-pixels [pixels-by-position]
    (map visible-pixel pixels-by-position))

(defn render-image [image]
    (doseq [row (partition 25 image)]
        (->> row
             (map #({0 " " 1 "*"} %))
             (str/join "")
             (println))))

(render-image (visible-pixels (stack-layers layers)))
; => LGYHB

