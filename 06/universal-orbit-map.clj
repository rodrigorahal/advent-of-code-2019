(require '[clojure.set :as set])
(require '[clojure.string :as str])
; graph:
; {
;    "COM" #{"B"}
;    "B" #{"G"}
;    "G" #{"H"}
;    ...    
;}

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn bfs-paths [graph start]
    (loop [seen #{start}
           to-see (queue [[start [start]]])
           paths {}]
        (if (empty? to-see)
            paths
            (let [[node path] (peek to-see)
                  new-seen (->> (graph node)
                             (filter #(not (contains? seen %)))
                             (set))
                  new-paths (for [u new-seen] [u (conj path node)])]
                  (recur
                    (set/union seen new-seen)
                    (reduce conj (pop to-see) new-paths)
                    (merge paths (into (hash-map) new-paths)))))))

(defn n-orbits [graph]
    (let [paths (bfs-paths graph "COM")]
        (->> paths
             (vals)
             (map #(dec (count %)))
             (reduce +))))

(defn read-input [filename]
    (with-open [rdr (clojure.java.io/reader filename)]
        (map #(str/split % #"\)") (reduce conj [] (line-seq rdr)))))

(defn make-graph [edges]
    (loop [graph {}
           e edges]
        (if (empty? e)
            graph
            (let [[v u] (first e)]
                    (recur (assoc graph v 
                                        (if (graph v)
                                            (conj (graph v) u)
                                            #{u})
                                        u
                                        (if (graph u)
                                            (conj (graph u) v)
                                            #{v}))
                            (rest e))))))

(assert (= (n-orbits (make-graph (read-input "test.txt"))) 42))

(println "Answer: " (n-orbits (make-graph (read-input "input.txt"))))

(defn shortest-path [graph start target]
    (loop [seen #{start}
           to-see (queue [start])
           dist-by-node {start 0}]
        (if (empty? to-see)
            (dist-by-node target)
            (let [node (peek to-see)
                  new-seen (set (filter #(not (contains? seen %)) (graph node)))
                  dists (for [u new-seen] [u (inc (dist-by-node node))])]
                  (recur
                    (set/union seen new-seen)
                    (reduce conj (pop to-see) new-seen)
                    (merge dist-by-node (into (hash-map) dists)))))))

(defn find-orbit [edges target]
    (->> edges
         (filter #(= (second %) target))
         (ffirst)))

(println "Answer: "
    (let [edges (read-input "input.txt")]
        (shortest-path 
            (make-graph edges)
            (find-orbit edges "YOU")
            (find-orbit edges "SAN"))))