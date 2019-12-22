(ns tractor-beam
  (:require [intcode :as ic] 
            [clojure.string :as str]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout poll! go-loop]]))



(defn read-input [filename]
  (vec (map read-string (str/split (slurp filename) #","))))

(def program (read-input "19/input.txt"))

(defn explore [program size]
    (let [
        points (for [y (range size)
                     x (range size)] [x y])]
        (go-loop [
            to-explore points
            grid {}]
            (if (empty? to-explore)
                grid
                (let [
                    [x y] (first to-explore)
                    [in out] (ic/run-intcode program)]
                  (>! in x)
                  (>! in y)
                  (let [status (<! out)
                        halt (<! out)]
                      (recur (rest to-explore)
                             (assoc grid [x y] status))))))))



(println "Part 1: " 
    (let [grid (<!! (explore program 50))]
        (->> grid
                (filter (fn [[k v]] (= v 1)))
                (count))))

(defn status [program position]
    (<!! (go (let [[x y] position
                   [in out] (ic/run-intcode program)]
                (>! in x)
                (>! in y)
                (let [s (<! out)
                      halt (<! out)]
                    s)))))

(defn fits? [program x y] (pos? (status program [(+ x 99) (- y 99)])))

(defn search [program]
  (loop [x 0 y 100]
    (if (= 1 (status program [x y]))
      (if (fits? program x y) (+ (* 10000 x) (- y 99))
          (recur x (inc y)))
      (recur (inc x) y))))

(println "Part 2: " (search program))

(defn draw [grid size]
    (let [
        points (for [y (range size)
                     x (range size)] [x y])
        image (map #(grid %) points)]
    (doseq [
        row (partition size image)]
        (->> row
             (map #(if (= 0 %) "." "#"))
             (str/join "")
             (println)))))





