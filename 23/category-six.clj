(ns categorysix
  (:require [intcode :as ic]
            [clojure.string :as str]))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn route-outputs
  [outputs]
  (->> outputs
       (flatten)
       (partition 3)
       (group-by first)
       (reduce-kv (fn [so-far k v] (assoc so-far k (flatten (map #(drop 1 %) v)))) {})))

(defn inputs->queue [inputs-by-destination i]
  (if (not (get inputs-by-destination i))
    (queue [-1])
    (queue (concat [-1] (vec (inputs-by-destination i))))))

(defn run-network [program size]
  (loop [states (map (fn [i] [program 0 0]) (range size))
         input-queues (map (fn [i] (queue [i -1])) (range size))]
    (let [results (map (fn [[state pos base] q] (ic/run-intcode-step state pos base q)) states input-queues)
          new-states (map first results)
          outputs (map second results)
          inputs-by-destination (route-outputs outputs)
          target (get inputs-by-destination 255)]
      (if target
        (second target)
        (let [new-input-queues (map (fn [i] (queue (get inputs-by-destination i (queue)))) (range size))]
          (recur new-states new-input-queues))))))

(defn run-network-with-nat [program size]
  (loop [states (map (fn [i] [program 0 0]) (range size))
         input-queues (map (fn [i] (queue [i -1])) (range size))
         last-seen-by-nat nil
         last-delivered-by-nat nil]
    (let [results (map (fn [[state pos base] q] (ic/run-intcode-step state pos base q)) states input-queues)
          new-states (map first results)
          outputs (map second results)
          inputs-by-destination (route-outputs outputs)
          sent-to-nat (last (partition 2 (get inputs-by-destination 255)))
          new-input-queues (map (fn [i] (inputs->queue inputs-by-destination i)) (range size))]
      (if (and (not sent-to-nat) (every? (fn [q] (= (vec q) [-1])) new-input-queues))
        (if (= (second last-seen-by-nat) last-delivered-by-nat)
          last-delivered-by-nat
          (recur new-states (assoc (vec new-input-queues) 0 (queue last-seen-by-nat)) nil (second last-seen-by-nat)))
        (recur new-states new-input-queues (or sent-to-nat last-seen-by-nat) last-delivered-by-nat)))))

(defn main []
  (def program (ic/allocate-additional-mem (ic/read-input "23/input.txt") 100))
  (println (format "Part 1: %d" (run-network program 50)))
  (println (format "Part 2: %d" (run-network-with-nat program 50))))

(main)

