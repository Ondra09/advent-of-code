(ns day_22.day22
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            ))

(use 'clojure.test)
;; (use 'clojure.contrib.cond)

(load-file "../util/simple2D.clj")
(clojure.core/alias 's2D 'util.simple2D)

(def input (->> (slurp "input-test2.txt")
                (str/trim)))

(defn parse-command [line]
  (do
    (if (re-matches #"deal into new stack" line) [:new-stack []]
        (if-let
            [re (re-find #"cut (\-?\d+)" line)] [:cut [(read-string (second re))]] ;; TODO parse to int directly
            (if-let
                [re (re-matches #"deal with increment (\-?\d+)" line)] [:deal [(read-string (second re))]]
                (do (println "Parsin failed: " line)
                    [:error []])
                )))))

(defn parse-input [coll]
  (->> coll
       (str/split-lines)
       (map parse-command)))

;; TODO: try to use transient for all
(defn deal-new-stack [coll]
  (reverse coll))

(defn cut-cards [coll n]
  (let [size (count coll)
        shift (mod n size)
        [head tail] (split-at shift coll)]
    (concat tail head)))

(deftest cut-cards-test
  (is (= (cut-cards [0 1 2 3 4 5 6 7 8 9] 2) [2 3 4 5 6 7 8 9 0 1]))
  (is (= (cut-cards [0 1 2 3 4 5 6 7 8 9] -4) [6 7 8 9 0 1 2 3 4 5]))
  (is (= (cut-cards [0 1 2 3 4 5 6 7 8 9] 3) [3 4 5 6 7 8 9 0 1 2])))

(defn increment-n [coll n]
  (loop [idx 0
         counter 0
         result (vec (repeat (count coll) 0))]
    (if (= counter (count coll)) result
        (recur (mod (+ idx n) (count coll)) (inc counter) (assoc result idx (nth coll counter))))))

;; (increment-n (range 10) 7)

(deftest shuffle-test
  (let [input-range (range 10)]
    (is (= (-> input-range
               (increment-n 7)
               (deal-new-stack)
               (deal-new-stack)) [0 3 6 9 2 5 8 1 4 7]))))

(defn symbol->fun [sym]
  (cond
    (= sym :cut) cut-cards
    (= sym :new-stack) deal-new-stack
    (= sym :deal) increment-n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (def result-1
;;   (reduce (fn [acc [symbol coll]] (do
;;                                     (println "sym: " symbol "coll: " coll)
;;                                     (apply (symbol->fun symbol) acc coll)))
;;           (range 10007)
;;           (parse-input input)))


(println "Part 1 result:"
         (.indexOf result-1 2019))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0 0                 0    0
;; 1 7                 7    3
;; 2 14                4    6
;; 3 21                1    9
;; 4 28                8    2
;; 5 35                5    5
;; 6 42                2    8
;; 7 49                9    1
;; 8 56                6    4
;; 9 63                3    7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; I gave up, this require too many repetitions found solution on reddit
;; it defines operations as modulo linear arithmetics
;; large number of repetitions is thus power of linear operations
(def deck-size 119315717514047)

;; very same as mine
(defn reverse-deal-new-stack-3 [idx count]
  (- count 1 idx))

;; similar to mine, but simplified and reversed
(defn reverse-cut-cards-3 [idx count cut]
  (mod (+ idx cut count) count))

;; https://rosettacode.org/wiki/Modular_inverse
(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(s2D/abs b) 0 1]
        (zero? b) [(s2D/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (s2D/abs b)
                     r0 (s2D/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn mod_inv
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


(defn reverse-increment-n-3 [idx count n]
  (let [mod_inv-val (bigint (mod_inv n count))]
    ;; (println mod_inv-val " --> " idx)
    (mod (* mod_inv-val idx) count)))


(defn symbol->fun-3 [sym]
  (cond
    (= sym :cut) reverse-cut-cards-3
    (= sym :new-stack) reverse-deal-new-stack-3
    (= sym :deal) reverse-increment-n-3))

;;
(def input-3 (reverse (parse-input input)))
;; ---------
(def n 101741582076661)

(defn  run-sim-3 [input idx n repetitions]
  (loop [idx idx
         repetitions repetitions]
    (cond (= repetitions 0) idx
          ;; (/ repetitions )
          :else (recur (reduce (fn [acc [symbol coll]]
                                 (apply (symbol->fun-3 symbol) acc n coll))
                               idx
                               input) (dec repetitions)))))

; (println "Result 3: " (long (run-sim-3 input-3 2020  deck-size n)))
