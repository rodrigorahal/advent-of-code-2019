; moons:
; [
;     {:id 0
;      :position [1 2 3]
;      :velocity [0 1 2]
;     }
;     {:id 1
;      :position [5 6 7]
;      :velocity [0 1 2]
;     }
; ]

(defn gravity [moon-a moon-b]
    (let [[xa ya za] (moon-a :position)
          [xb yb zb] (moon-b :position)
          [vxa vya vza] (moon-a :velocity)]
        (assoc moon-a 
               :velocity
               [
                   (if (> xa xb) (dec vxa) (if (< xa xb) (inc vxa) vxa))
                   (if (> ya yb) (dec vya) (if (< ya yb) (inc vya) vya))
                   (if (> za zb) (dec vza) (if (< za zb) (inc vza) vza))
               ])))

(defn update-velocity [moon moons]
    (->> moons
         (filter #(not= (moon :id) (% :id)))
         (reduce gravity moon)))

(defn update-position [moon]
    (let [position (into []
                         (map #(apply + %)
                               (map vector (moon :position) (moon :velocity))))]
        (assoc moon :position position)))

(defn run-step [moons]
    (map update-position
         (map #(update-velocity % moons) moons)))

(defn simulate [initial-state steps]
    (loop [moons initial-state
           step 0]
        (if (= step steps)
            moons
            (recur 
                (run-step moons)
                (inc step)))))

(defn dim [moon f]
    (-> moon
        (assoc :position (f (moon :position)))
        (assoc :velocity (f (moon :velocity)))))

(defn x [moon]
    (dim moon first))

(defn y [moon]
    (dim moon second))

(defn z [moon]
    (dim moon (fn [item] (nth item 2))))

(defn update-cycle-step [state seen cycle-step step]
    (cond 
        cycle-step cycle-step
        (contains? seen state) step
        :else nil))

(defn find-previous [initial-state]
    (loop [moons initial-state
           step 0
           seen-x (into #{} (map x moons))
           seen-y (into #{} (map y moons))
           seen-z (into #{} (map z moons))
           cycle-step-x nil
           cycle-step-y nil
           cycle-step-z nil]
        (let [new-state (run-step moons)
              new-x-state (map x new-state)
              new-y-state (map y new-state)
              new-z-state (map z new-state)]
            (cond 
                (and cycle-step-x cycle-step-y cycle-step-z) [cycle-step-x cycle-step-y cycle-step-z]
                :else (recur new-state
                             (inc step)
                             (conj seen-x new-x-state)
                             (conj seen-y new-y-state)
                             (conj seen-z new-z-state)
                             (update-cycle-step new-x-state seen-x cycle-step-x step)
                             (update-cycle-step new-y-state seen-y cycle-step-y step)
                             (update-cycle-step new-z-state seen-z cycle-step-z step))))))

(defn gcd [a b]
    (if (zero? b)
        a
        (recur b (mod a b))))

(defn lcm 
    ([a b] (/ (* a b) (gcd a b)))
    ([a b c] (lcm a (lcm b c))))

(defn pot [moon]
    (->> (moon :position)
         (map #(Math/abs %))
         (reduce +)))

(defn kin [moon]
    (->> (moon :velocity)
         (map #(Math/abs %))
         (reduce +)))

(defn energy [moon]
    (* (pot moon) 
       (kin moon)))

(defn total-energy [moons]
    (->> moons
         (map energy)
         (reduce +)))

(def moons [
    {:id 0 :position [-1 0 2] :velocity [0 0 0]}
    {:id 1 :position [2 -10 -7] :velocity [0 0 0]}
    {:id 2 :position [4 -8 8] :velocity [0 0 0]}
    {:id 3 :position [3 5 -1] :velocity [0 0 0]}
])

(assert  (= (total-energy (simulate moons 10)) 179))

(assert (= (apply lcm (find-previous moons)) 2772))

(def moons [
    {:id 0 :position [-8 -10 0] :velocity [0 0 0]}
    {:id 1 :position [5 5 10] :velocity [0 0 0]}
    {:id 2 :position [2 -7 3] :velocity [0 0 0]}
    {:id 3 :position [9 -8 -3] :velocity [0 0 0]}
])

(assert (= (total-energy (simulate moons 100)) 1940))

(assert (= (apply lcm (find-previous moons)) 4686774924))

(def moons [
    {:id 0 :position [14 4 5] :velocity [0 0 0]}
    {:id 1 :position [12 10 8] :velocity [0 0 0]}
    {:id 2 :position [1 7 -10] :velocity [0 0 0]}
    {:id 3 :position [16 -5 3] :velocity [0 0 0]}
])

(println "Part 1: " (total-energy (simulate moons 1000)))

(println "Part 2: " (apply lcm (find-previous moons)))


    
