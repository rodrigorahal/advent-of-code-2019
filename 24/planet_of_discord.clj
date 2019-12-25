
(defn tile-value [pos grid]
    (get grid pos "."))

(defn bug? [pos grid]
    (= "#" (tile-value pos grid)))

(defn space? [pos grid]
    (= "." (tile-value pos grid)))

(defn neighbors [[x y]]
    (filter (fn [[x y]] (and (<= 0 x 4)
                             (<= 0 y 4)))
            [
                [(inc x) y]
                [(dec x) y]
                [x (inc y)]
                [x (dec y)]
            ]))

(defn bugs [neighbors grid]
    (->> neighbors
         (filter #(bug? % grid))
         (count)))

(defn update-tile [[x y] grid]
    (let [ngbrs (neighbors [x y]) 
          bgs (bugs ngbrs grid)]
        (cond 
            (bug? [x y] grid)
            (if (= bgs 1)
                [[x y] "#"]
                [[x y] "."])
            
            (space? [x y] grid)
            (if (<= 1 bgs 2)
                [[x y] "#"]
                [[x y] "."]))))

(defn step [grid]
    (->> grid
         (keys)
         (map #(update-tile % grid))
         (into {})))

(defn simulate [grid]
    (loop [grid' grid
           seen #{grid}]
        (let [next-grid (step grid')]
            (if (contains? seen next-grid)
                next-grid
                (recur next-grid
                       (conj seen next-grid))))))

(defn draw [grid]
    (let [points (for [y (range 5) x (range 5)] [x y])
          image (map grid points)
          rows (partition 5 image)]

        (doseq [row rows]
            (->> row
                 (clojure.string/join "")
                 (println)))))

(defn biodiversity [grid]
    (let [points (for [y (range 5) x (range 5)] [x y])]
        (->> points
            (map-indexed (fn [idx pnt] (when (bug? pnt grid)
                                             (Math/pow 2 idx))))
            (remove nil?)
            (reduce +)
            (int))))

(defn read-input [filename]
    (with-open [rdr (clojure.java.io/reader filename)]
        (let [rows (line-seq rdr)]
            (loop [row nil
                   rows rows
                   y 0
                   grid {}]
                (if (empty? rows)
                    grid
                    (recur (first rows)
                           (rest rows)
                           (inc y)
                           (merge grid 
                                  (into {} 
                                        (map-indexed (fn [x itm] [[x y] (str itm)]) (first rows))))))))))


(defn recursive-neighbors [[x y z]]
    (let [nbrs (remove (fn [[x y]] (and (= x 2) (= y 2))) (neighbors [x y]))
          rnbrs (map #(conj % z) nbrs)]

        (cond (contains? #{[1 1] [3 1] [1 3] [3 3]} [x y]) 
            rnbrs

            (and (= y 0)
                (and (not= x 0) (not= x 4)))
            (conj rnbrs [2 1 (dec z)])

            (and (= y 4)
                    (and (not= x 0) (not= x 4)))
            (conj rnbrs [2 3 (dec z)])
            
            (and (= x 0)
                 (and (not= y 0) (not= y 4)))
            (conj rnbrs [1 2 (dec z)])


            (and (= x 4)
                 (and (not= y 0) (not= y 4)))
            (conj rnbrs [3 2 (dec z)])
            
            (and (= x 0) (= y 0))
            (concat rnbrs [
                [2 1 (dec z)]
                [1 2 (dec z)]])
            
            (and (= x 4) (= y 0))
            (concat rnbrs [
                [2 1 (dec z)]
                [3 2 (dec z)]])
            
            (and (= x 0) (= y 4))
            (concat rnbrs [
                [1 2 (dec z)]
                [2 3 (dec z)]])


            (and (= x 4) (= y 4))
            (concat rnbrs [
                [2 3 (dec z)]
                [3 2 (dec z)]])
            
            (and (= x 2) (= y 1))
            (concat rnbrs
                    (for [x (range 5)] [x 0 (inc z)]))

            (and (= x 1) (= y 2))
            (concat rnbrs
                    (for [y (range 5)] [0 y (inc z)]))

            (and (= x 2) (= y 3))
            (concat rnbrs
                    (for [x (range 5)] [x 4 (inc z)]))

            (and (= x 3) (= y 2))
            (concat rnbrs
                    (for [y (range 5)] [4 y (inc z)])))))

(defn recursive-update-tile [[x y z] grid]
    (let [nbrs (recursive-neighbors [x y z])
          bgs (bugs nbrs grid)]

        (cond (bug? [x y z] grid)
              (if (= bgs 1)
                  [[x y z] "#"]
                  [[x y z] "."])
              
              (space? [x y z] grid)
              (if (<= 1 bgs 2)
                  [[x y z] "#"]
                  [[x y z] "."]))))

(defn levels [grid]
    [
        (apply min (map (fn [[x y z]] z) (keys grid)))
        (apply max (map (fn [[x y z]] z) (keys grid)))
    ])

(defn step-3d [grid]
    (let [[minz maxz] (levels grid)]
        (into {} 
              (for [y (range 5)
                    x (range 5)
                    z (range (dec minz) (+ maxz 2))]
                    (recursive-update-tile [x y z] grid)))))

(defn find-bugs [grid]
    (loop [grid grid
           step 0]
        (if (= step 200)
            grid
            (let [[minz maxz] (levels grid)]
                (recur 
                    (into {} 
                        (for [y (range 5)
                            x (range 5)
                            z (range (dec minz) (+ 2 maxz))]
                            (recursive-update-tile [x y z] grid)))
                    (inc step))))))

(def grid-2d (read-input "24/input.txt"))

(def grid-3d 
    (into {} 
          (map (fn [ [[x y] v] ] [[x y 0] v])
               grid-2d)))

(defn level [grid-3d z]
    (->> grid-3d
         (map (fn [ [[x y z'] v] ] (when (= z' z) [[x y] v])))
         (remove nil?)
         (into {})))

(defn draw-3d [grid-3d]
    (let [[minz maxz] (levels grid-3d)]
        (doseq [z (range minz (inc maxz))]
            (println "level: " z)
            (draw (level grid-3d z)))))

(defn total-bugs [grid]
    (->> grid
         (vals)
         (filter #(= % "#"))
         (count)))


(defn main []
    (println "Part 1: " (biodiversity (simulate grid-2d)))

    (println "Part 2: " (total-bugs (find-bugs grid-3d))))





