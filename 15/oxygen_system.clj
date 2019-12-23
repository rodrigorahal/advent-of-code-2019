(ns oxygen-system
  (:require [intcode :as ic] 
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout poll! go-loop]]))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(def opposites 
    {
        1 2 ;; north to south
        2 1 ;; south to north
        3 4 ;; west to east
        4 3 ;; east to west
    })

(defn opposite [move]
    (opposites move))

(defn neighbors [[x y]]
    [
        [[x (inc y)] 1] ;; north
        [[x (dec y)] 2] ;; south
        [[(inc x) y] 3] ;; east
        [[(dec x) y] 4] ;; west
    ]
)

(defn update-position [[x y] move]
    (cond 
        (= move 1) [x (inc y)]
        (= move 2) [x (dec y)]
        (= move 3) [(inc x) y]
        (= move 4) [(dec x) y]))


(defn open-neighbors [in out pos moves]
    (let [status (atom [])]
        (<!! (go 
                (doseq [m moves]
                    (>! in m)
                    (let [s (<!! out)]
                        (when (pos? s)
                            (do (swap! status conj [pos m])
                                (>! in (opposite m))
                                (<! out)))))
        @status))))


;; explore the world in dfs fashion
;; backtracks when can't go further
(defn dfs [program start] 
    (let [[in out] (ic/run-intcode program)]
    
        (go-loop [
            pos start
            visited #{start}
            stack (list [start 2])
            backtrack '()
            oxygen nil]
        (if (empty? stack)
            [visited oxygen]
            (let [[from move] (peek stack)]
                (if (= from pos) ;; can move to next position in the stack
                    ;; move to position and update visited
                    (doseq []
                        (>! in move) ;; update pos in intcode
                        (let [
                            status (<! out)
                            pos' (update-position pos move)
                            oxygen' (if (= status 2) pos' oxygen)
                            backtrack' (conj backtrack (opposite move))
                            visited' (conj visited pos')
                            not-visited-neighbors (filter (fn [[p m]] (not (contains? visited p))) (neighbors pos')) ;; neighbors we can visit
                            moves-to-not-visited (map second not-visited-neighbors) ;; move to get there
                            neighbors' (open-neighbors in out pos' moves-to-not-visited) ;; filter neighbors that are not wall
                        ]
 
                        (recur
                            pos'
                            visited'
                            (into '() (concat (pop stack) neighbors'))
                            backtrack'
                            oxygen')))

                    ;; can't move to the next position in the stack, it was originated from a different position
                    ;; backtrack
                    (let [
                        back-move (peek backtrack)
                        pos' (update-position pos back-move)]

                        (doseq []
                            (>! in back-move) ;; update position in intcode
                            (<! out)
                            (recur
                                pos'
                                visited
                                stack
                                (pop backtrack)
                                oxygen)))))))))


(defn shortest-path [nodes start target]
    (loop [visited #{start}
           to-visit (queue [start])
           dist-by-node {start 0}]
        (if (empty? to-visit)
            (dist-by-node target)
            (let [node (peek to-visit)

                  neighbors' (->> node
                                  (neighbors)
                                  (map first)
                                  (filter #(contains? nodes %))
                                  (filter #(not (contains? visited %)))
                                  (set))

                  dist-by-node' (into {} (for [u neighbors'] [u (inc (dist-by-node node))]))]

                  (recur
                    (set/union visited neighbors')
                    (reduce conj (pop to-visit) neighbors')
                    (merge dist-by-node dist-by-node'))))))


(defn read-input [filename]
  (vec (map read-string (str/split (slurp filename) #","))))

(def program (read-input "15/input.txt"))

(defn main []
    (let [
        [visited oxygen] (<!! (dfs program [0 0]))]

        (println "Part 1: " (shortest-path visited [0 0] oxygen))

        (println "Part 2: " (apply max (for [v visited] (shortest-path visited v oxygen))))))
