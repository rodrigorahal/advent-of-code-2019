(ns slam-shuffle
    (:require [clojure.math.numeric-tower :as math]
              [clojure.string :as str]))

(defn deal-into-new-stack [cards]
    (into [] (reverse cards)))

(defn cut [n cards]
    (cond (pos? n)
          (into [] (concat (subvec cards n)
                           (subvec cards 0 n)))
          (neg? n)
          (let [idx (+ (count cards) n)]
            (into [] (concat (subvec cards idx)
                             (subvec cards 0 idx))))))

(defn deal-with-increment [n cards]
    (let [deck-size (count cards)]
        (loop [to-deal cards           
               dealt (into [] (repeat deck-size nil))
               idx 0]
        (if (empty? to-deal)
            dealt
            (recur (rest to-deal)
                   (assoc dealt (rem idx deck-size) (first to-deal))
                   (+ idx n))))))

(def instruction->operation 
    {
        "deal-into-new-stack" deal-into-new-stack
        "cut" cut
        "deal-with-increment" deal-with-increment
    }
)

(defn parse-instruction [s]
    (cond (= s "deal into new stack")
          [s deal-into-new-stack]

          (str/starts-with? s "cut")
          (let [n (->> s
                       (drop 4)
                       (str/join "")
                       (read-string))]
            [s (partial cut n)])
          :else 
          (let [n (->> s
                        (drop 20)
                        (str/join "")
                        (read-string))]
            [s (partial deal-with-increment n)])))

(defn read-input [filename parse]
    (with-open [rdr (clojure.java.io/reader filename)]
        (map parse (reduce conj [] (line-seq rdr)))))

(defn deal [cards instructions]
    (loop [cards cards
           instructions instructions]
        (if (empty? instructions)
            cards
            (let [[name operation] (first instructions)]
                (recur (operation cards)
                       (rest instructions))))))

(defn main []
    (def cards (into [] (range 10007)))
    (def instructions (read-input "22/input.txt" parse-instruction))

    (println "Part1 : "
        (.indexOf (deal cards instructions)
                  2019)))


;; Part 2

(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))
 
(defn modinv
  " Get inverse using extended gcd.  Extended GCD returns
    gcd followed by bezout coefficients. We want the 1st coefficients
   (i.e. second of extend-gcd result).  We compute mod base so result
    is between 0..(base-1) "
  [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (extended-gcd a b)]
      (if (= (first egcd) 1)
        (mod (second egcd) b)
        (str "No inverse since gcd is: " (first egcd)))))
 

(defn reverse-deal [i D]
    (-' D 1 i))

(defn reverse-cut [N i D]
    (mod (+' i N D) D))

(defn reverse-increment [N i D]
    (mod (*' i
            (modinv N D)) 
         D))

(defn parse-reverse-instruction [s]
    (cond (= s "deal into new stack")
          [s reverse-deal]

          (str/starts-with? s "cut")
          (let [n (->> s
                       (drop 4)
                       (str/join "")
                       (read-string))]
            [s (partial reverse-cut n)])
          :else 
          (let [n (->> s
                        (drop 20)
                        (str/join "")
                        (read-string))]
            [s (partial reverse-increment n)])))

(def reverse-instructions (read-input "22/input.txt" parse-reverse-instruction))

(defn reverse-deck [i D reverse-instructions]
    (loop [instructions (reverse reverse-instructions)
           x i]
        (if (empty? instructions)
            x
            (let [[name operation] (first instructions)
                  x' (operation x D)]
                (recur (rest instructions)
                       x')))))

(defn part-2 []
    (def X 2020)
    (def D 119315717514047) ;; deck size
    (def Y (reverse-deck X D reverse-instructions)) ;; one time reversion
    (def Z (reverse-deck Y D reverse-instructions)) ;; two time reversion

    ;; f(i) = A * i + B
    (def A (mod (* (- Y Z) 
                   (modinv (+ D (- X Y)) 
                           D)) 
                D))
    (def B (mod (- Y (* A X)) D))
    
    (def n 101741582076661)
    
    ;; apply f(i) n times
    (def res 
        (mod (+'
                (*' X
                    (.modPow (biginteger A) (biginteger n) (biginteger D)))
                (*' (-' (.modPow (biginteger A) (biginteger n) (biginteger D)) 1)
                    (* (modinv (- A 1) D) B))) 
             D))
    
    (println "Part 2: " res))




