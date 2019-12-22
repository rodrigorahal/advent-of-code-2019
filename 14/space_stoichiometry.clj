(require '[clojure.string :as str])

(defn process [chem reactions]
    (let [
        produces ((reactions chem) :produces)
        n (long (Math/ceil (/ (max 0 (- (get @required chem 0) (get @produced chem 0)))
                           produces)))
    ]
    (swap! produced 
           assoc chem 
                 (+ (get @produced chem 0) (* n produces)))
    (doseq [r ((reactions chem) :reactions)]
        (swap! required 
               assoc (r :chem) 
                     (+ (get @required (r :chem) 0) (* n (r :req)))))
    
    (doseq [r ((reactions chem) :reactions)]
        (when (not= (r :chem) "ORE")
            (process (r :chem) reactions)))))

(defn parse-reaction [row]
    (let [reagents (into [] (map (fn [[_ num chem]] {:req (read-string num) :chem chem})
                                 (re-seq #"(\d+) (\w+)" row)))
          chem (:chem (last reagents))
          produces (:req (last reagents))]
        [chem {:reactions (butlast reagents) :produces produces}]))

(defn read-input [filename]
    (with-open [rdr (clojure.java.io/reader filename)]
        (into {} (map parse-reaction (reduce conj [] (line-seq rdr))))))

(def reactions (read-input "14/input.txt"))

(defn ore-to-produce-fuel [qty reactions]
    (def required (atom {}))
    (def produced (atom {}))
    (swap! required assoc "FUEL" qty)
    (process "FUEL" reactions)
    (@required "ORE"))

(println "Part 1: " (ore-to-produce-fuel 1 reactions))


(defn search [reactions cargo]
    (loop [lo 1
           hi cargo]
        (if (>= lo hi)
            (dec lo)
            (let [fuel (long (/ (+ lo hi) 2))
                  ore (ore-to-produce-fuel fuel reactions)]
                (if (<= ore cargo)
                    (recur (+ fuel 1) hi)
                    (recur lo (- fuel 1)))))))

(println "Part 2: " (search reactions 1000000000000))


