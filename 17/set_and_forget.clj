(ns set-and-forget
  (:require [intcode :as ic] 
            [clojure.string :as str]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout poll! go-loop]]))

(defn world-map [program]
    (let [[in out] (ic/run-intcode program)]
        (<!! (go-loop [chars []
                       step 0]
                (if (< step 3121)
                    (let [code (<! out)]
                        (recur (conj chars (char code))
                            (inc step)))
                    chars)))))

(defn read-and-print [out]
    (go 
        (while true
            (let [code (poll! out)]
                (when code
                      (print (char code)))))))

(defn draw [chars]
    (doseq [c chars] (print c))
    (println)
    chars)

(defn grid [chars]
    (let [rows (str/split (str/join "" chars) #"\n")]
        (loop [row (first rows)
               rows (rest rows)
               y 0
               grid {}]
            (if (empty? rows)
                grid
                (recur (first rows)
                       (rest rows)
                       (inc y)
                       (merge grid 
                             (into {} (remove nil? (map-indexed (fn [x item] (when (= item \#) [[x y] item])) row)))))))))

(defn intersection? [[x y] grid]
    (and (grid [(inc x) y])
         (grid [x (inc y)])
         (grid [(dec x) y])
         (grid [x (dec y)])))

(defn intersections [grid]
    (filter #(intersection? % grid) (keys grid)))

(def program (ic/read-input "17/input.txt"))

(defn main []
    (->> program
        (world-map)
        (draw)
        (grid)
        (intersections)
        (map #(apply * %))
        (reduce +)
        (println "Part 1: ")))


;; I did Part 2 manually.
;; From map.txt I got the sequence in path.txt.
;; And derived Main and Function also in path.txt.
;; Then Ran ic/intcode interactively in the repl to get:

(println "Part 2: " 1499679)

