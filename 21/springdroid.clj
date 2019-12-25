(ns springdroid
  (:require [intcode :as ic] 
            [clojure.string :as str]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout poll! go-loop]]))


(def program (ic/read-input "21/input.txt"))


(defn read-and-print [out]
    (go-loop [read? :read]
        (let [code (<! out)
              chr (try (char code) (catch IllegalArgumentException e false))]
            (if (false? chr)
                code
                (do (print chr)
                    (recur read?))))))

(defn str->chars [s]
    (->> s
         (.getBytes)
         (map char)))

(defn write [c chr]
    (>!! c (int chr)))

(defn write-instruction [c instruction]
    (let [chrs (str->chars instruction)]
        (doseq [chr chrs]
            (write c chr))))

(def walk-springscript [
    "NOT A J"
    "NOT B T"
    "OR T J"
    "NOT C T"
    "OR T J"
    "AND D J"
    "WALK"])

(def run-springscript [
    "NOT A J"
    "NOT B T"
    "OR T J"
    "NOT C T"
    "OR T J"
    "AND D J"
    "NOT H T"
    "NOT T T"
    "OR E T"
    "AND T J"
    "RUN"
])

(defn run-droid [script]
    (let [[in out] (ic/run-intcode program)]
        (def damage-out (read-and-print out))
        (doseq [instruction script]
            (write-instruction in instruction)
            (write in \newline))
        (println "Damage: " (<!! damage-out))))

(defn main []
    (run-droid walk-springscript)

    (run-droid run-springscript))