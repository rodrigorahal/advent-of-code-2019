(ns care-package
  (:require [intcode :as ic] 
            [clojure.string :as str]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout poll! go-loop]]))


(def id->tile
    {
        0 " " ;; empty
        1 "#" ;; wall
        2 "=" ;; block
        3 "_" ;; paddle
        4 "*" ;; ball
    }
)

;; tile 
;; { :x 0 :y 2 :id 0 }

;; screen: [x y] -> tile
;; {
;;     [0 0] " "
;;     [1 0] "*" 
;; }

(defn add-to-screen [screen tile]
    (assoc screen [(tile :x) (tile :y)] (id->tile (tile :id))))

(defn draw-game [screen]
    (let [
        min-x (apply min (map #(first %) (keys screen)))
        max-x (apply max (map #(first %) (keys screen)))
        min-y (apply min (map #(second %) (keys screen)))
        max-y (apply max (map #(second %) (keys screen)))

        points (for [y (range min-y (inc max-y)) x (range min-x (inc max-x))] [x y])
        image (map #(or (screen %) 0) points)]

        (print (str (char 27) "[2J")) ; clear screen
        (print (str (char 27) "[;H")) ; move cursor to the top left corner of the screen

        (doseq [row (partition (- (inc max-x) min-x) image)]
        (->> row
        (map #(id->tile %))
        (str/join "")
        (println)))))

(defn move-paddle [paddle ball]
    (let [
        [xp yp] paddle
        [xb yb] ball]

        (cond (= xp xb) 0
              (> xp xb) -1
              (< xp xb) 1)))

(defn halt? [code]
    (= code 99))

(defn score? [code]
    (= code -1))

(defn ball? [id]
    (= id 4))

(defn paddle? [id]
    (= id 3))

(defn run-game [program & {:keys [draw] :or {draw false}}]
    (let [[in out] (ic/run-intcode program)
          screen (atom {})]
        (go-loop [score 0
                  paddle nil
                  ball nil]
            (let [code (<! out)]
                (cond (halt? code)
                        score 
                      (score? code)
                        (let [_ (<! out)
                              next-score (<! out)]
                            (when (= next-score 0) ;; game start
                                (do (>! in (move-paddle paddle ball))
                                        (when draw (draw-game @screen))))
                                (recur next-score paddle ball))
                      :else
                        (let [x  code
                              y  (<! out)
                              id (<! out)]
                              (swap! screen assoc [x y] id)
                              (when
                                (and (ball? id)
                                     (not (nil? paddle)))
                                (do (>! in (move-paddle paddle [x y]))
                                      (when draw (do (draw-game @screen)
                                      (Thread/sleep 50)))))             
                              (recur score
                                     (if (paddle? id) [x y] paddle)
                                     (if (ball? id) [x y] ball))))))))

(defn num-block-tiles [screen]
    (count (filter #(= 2 %) (vals screen))))

(defn read-input [filename]
  (vec (map read-string (str/split (slurp filename) #","))))

(def program (read-input "13/input.txt"))

(def program (assoc program 0 2))

(defn main []
    ;; usage (run-game program :draw true)
    (println "Score: " (<!! (run-game program))))
