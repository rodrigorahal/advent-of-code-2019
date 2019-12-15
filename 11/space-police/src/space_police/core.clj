(ns space-police.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout poll!]]))

(require '[clojure.string :as str])

(def opcode-to-op 
  {1 (fn [a b] (+ a b))           ;sum
   2 (fn [a b] (* a b))           ;product
   7 (fn [a b] (if (< a b) 1 0))  ;less-than
   8 (fn [a b] (if (= a b) 1 0))  ;equals
  })

(def instruction-size-by-opcode {1 4, 2 4, 3 2, 4 2, 5 3, 6 3, 7 4, 8 4, 9 2})

(defn jump? [opcode] (contains? #{5 6} opcode))

(defn halt? [opcode] (= opcode 99))

(defn digits [num]
  (->> (str num)
       (map str)
       (map read-string)))

(defn instruction->opcode [inst]
    (cond (= (count (str inst)) 1) inst
          (= (count (str inst)) 2) inst
          :else (read-string (str (last (str inst))))))      

(defn instruction->modes [inst]
  (let [opcode (instruction->opcode inst)
        digs (digits inst)]
    (cond (= (count digs) 1) (repeat (dec (instruction-size-by-opcode opcode)) 0)
          :else (concat
                  (subvec (vec (reverse digs)) 2)
                  (repeat (- (dec (instruction-size-by-opcode opcode)) (- (count digs) 2)) 0)))))

(defn instruction->parameters [opcode state pos]
    (subvec state
            (inc pos)
            (+ pos (instruction-size-by-opcode opcode))))

(defn parameter-value [state param mode rel-base]
    (cond (= mode 0) (state param)
          (= mode 1) param
          (= mode 2) (state (+ rel-base param))
          :else (println "Unsupported mode ", mode)))

(defn parameter-address [param mode rel-base]
    (cond 
        (= mode 2) (+ rel-base param)
        :else param))

(defn update-rel-base [rel-base state opcode params modes]
    (cond 
        (= opcode 9) (+ rel-base (parameter-value state (first params) (first modes) rel-base))
        :else rel-base))

(defn update-pointer [pos state opcode params modes rel-base]
  (cond 
    (= opcode 5) 
      (if (not (zero? (parameter-value state (first params) (first modes) rel-base))) 
          (parameter-value state (second params) (second modes) rel-base)
          (+ pos 3))
    (= opcode 6)
      (if (zero? (parameter-value state (first params) (first modes) rel-base)) 
          (parameter-value state (second params) (second modes) rel-base)
          (+ pos 3))
    :else (->> opcode
               (instruction-size-by-opcode)
               (+ pos))))

(defn update-state [state opcode params modes rel-base in-chan out-chan]
    (cond 
        (jump? opcode) state
        (= opcode 9) state
        (= opcode 3) (let [read (<!! in-chan)]
                        (assoc state 
                               (parameter-address (first params) (first modes) rel-base)
                               read))
        (= opcode 4) (let [output (parameter-value state (first params) (first modes) rel-base)]
                            (>!! out-chan output)
                            state)
        :else (let [[i j target] params
                    [mi mj mt] modes]
                        (assoc state 
                               (parameter-address target mt rel-base)
                               ((opcode-to-op opcode) 
                                    (parameter-value state i mi rel-base)
                                    (parameter-value state j mj rel-base))))))

(defn run-intcode [initial-state in-chan out-chan]
  (loop [state initial-state
         pos 0
         rel-base 0]
        (let [instruction (state pos)
              opcode (instruction->opcode instruction)]
          (if (halt? opcode)
              (do (close! in-chan)       ;; close the channel
                  (poll! in-chan)        ;; drain the channel to unblock writers
                  (>!! out-chan opcode)) ;; communicate termination
              (let [params (instruction->parameters opcode state pos)
                    modes (instruction->modes instruction)]
                (recur (update-state state opcode params modes rel-base in-chan out-chan)
                       (update-pointer pos state opcode params modes rel-base)
                       (update-rel-base rel-base state opcode params modes)))))))

; examples:
; state [1002 4 3 4 33]
; inst 1002
; parameters [4 3 4]
; modes (0 1 0)
(defn read-input [filename]
  (vec (map read-string (str/split (slurp filename) #","))))

(defn allocate-additional-mem [initial-state size]
    (into [] (concat initial-state (repeat size 0))))
  
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

(defn run-robot [start-panel-color in-chan out-chan]
  (loop [position [0 0]
         direction :up
         color-by-position {position start-panel-color}]
      (>!! out-chan (color-to-code (or (color-by-position position) :black)))
      (let [code (<!! in-chan)]
        (if (= code 99) ;; halt
            (do
              (println "count: " (count (keys color-by-position)))
              color-by-position)
            (let [paint-code code
                  paint-color (code-to-color paint-code)
                  turn-code (<!! in-chan)
                  turn-direction (code-to-turn-direction turn-code)
                  new-direction (turn direction turn-direction)
                  new-position (move position new-direction)]
          (recur new-position
                 new-direction
                 (assoc color-by-position position paint-color)))) )))

(defn -main
  "Will run robot with channels."
  [& args]
  (def initial-state (read-input "resources/input.txt"))
  (def robot-to-cpu (chan))
  (def cpu-to-robot (chan))
  (go (run-intcode (allocate-additional-mem initial-state (* 100 (count initial-state))) 
                   robot-to-cpu cpu-to-robot))
  (draw (<!! (go (run-robot :white cpu-to-robot robot-to-cpu))))
)

;   XX  XXX  XXXX X  X XXX  X  X XXX  XXX
;  X  X X  X X    X X  X  X X X  X  X X  X
;  X  X X  X XXX  XX   X  X XX   XXX  X  X
;  XXXX XXX  X    X X  XXX  X X  X  X XXX
;  X  X X    X    X X  X X  X X  X  X X X
;  X  X X    X    X  X X  X X  X XXX  X  X
