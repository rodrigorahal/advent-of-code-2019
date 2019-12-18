(ns space-police
  (:require [intcode :as ic] 
            [clojure.string :as str]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout poll! go-loop]]))

(defn turn [direction turn-direction] 
  (let [turns {
                :up [:left :right]
                :down [:right :left]
                :right [:up :down]
                :left [:down :up]}]
    (cond 
      (= turn-direction :left) (first (turns direction))
      (= turn-direction :right) (second (turns direction)))))

(defn go-direction [direction] 
  ({
    :up [0 1]
    :left [-1 0]
    :down [0 -1]
    :right [1 0]
  } direction))

(defn color-to-code [color]
  ({:black 0 :white 1} color))

(defn code-to-color [code]
  ({0 :black 1 :white} code))

(defn code-to-turn-direction [code]
  ({0 :left 1 :right} code))

  
(defn move [position new-direction]
  (into []
        (map #(apply + %)
             (map vector position (go-direction new-direction)))))

(defn draw [color-by-position]
  (let [
    min-x (apply min (map #(first %) (keys color-by-position)))
    max-x (apply max (map #(first %) (keys color-by-position)))
    min-y (apply min (map #(second %) (keys color-by-position)))
    max-y (apply max (map #(second %) (keys color-by-position)))

    points (for [y (range max-y (dec min-y) -1) x (range min-x (inc max-x))] [x y])
    image (map #(or (color-by-position %) :black) points)]

    (doseq [row (partition (- (inc max-x) min-x) image)]
      (->> row
        (map #({:black " " :white "X"} %))
        (str/join "")
        (println)))))

(defn run-robot [program start-panel-color]
  (let [
    [robot-to-cpu cpu-to-robot] (ic/run-intcode program)]
    (<!! 
      (go-loop [position [0 0]
                direction :up
                color-by-position {position start-panel-color}]
        (>! robot-to-cpu (color-to-code (or (color-by-position position) :black)))
        (let [code (<! cpu-to-robot)]
          (if (= code 99) ;; halt
              color-by-position
              (let [paint-code code
                    paint-color (code-to-color paint-code)
                    turn-code (<! cpu-to-robot)
                    turn-direction (code-to-turn-direction turn-code)
                    new-direction (turn direction turn-direction)
                    new-position (move position new-direction)]
            (recur new-position
                  new-direction
                  (assoc color-by-position position paint-color)))))))))

(defn read-input [filename]
  (vec (map read-string (str/split (slurp filename) #","))))

(defn main []
  (def initial-state (read-input "11/input.txt"))
  (println "Part 1: " (count (keys (run-robot initial-state :black))))
  (draw (run-robot initial-state :white)))
  

(main)