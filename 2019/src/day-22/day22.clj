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

(def input (->> (slurp "input.txt")
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
(def result-1
  (reduce (fn [acc [symbol coll]] (do
                                    (println "sym: " symbol "coll: " coll)
                                    (apply (symbol->fun symbol) acc coll)))
          (range 10007)
          (parse-input input)))


(println "Part 1 result:"
         (.indexOf result-1 2019))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2
;;
;; Part 1 is naive and unoptimal, it runs for minutes or two
;; even if this would be optimizet it is unpractical do tompute everything in part 2
;; Two ideas how to make part 2 are
;; 1) reduce input to remove instrucitons e.g. if we have reverse; cut n; reverse
;;    we can simplify it to single instruction only: cut -n; however this would
;;    pbly be still impractical as input is very large
;;
;; 2) result wants to only position of one number to be computed;
;;    so we will rewrite all three functions to recompute position of only one item

(defn deal-new-stack-2 [idx count]
  (- (dec count) idx))

(deftest deal-new-stack-2-test
  (is (= (deal-new-stack-2 9 10) 0))
  (is (= (deal-new-stack-2 0 10) 9))
  (is (= (deal-new-stack-2 1 10) 8))
  (is (= (deal-new-stack-2 0 7) 6))
  (is (= (deal-new-stack-2 3 7) 3)))


(defn cut-cards-2 [idx count cut]
  (let [shift (mod cut count)
        diff (- count shift)
        ]
    (mod (+ idx diff) count)))

(defn cuts-same? [idx count cut]
  (=
   (cut-cards-2 idx count cut)
   (.indexOf (cut-cards (range count) cut) idx)))

(deftest cut-cards-2-test
  (is (= (cut-cards-2 7 10 0) 7))
  (is (= (cut-cards-2 0 10 0) 0))
  (is (= (cut-cards-2 3 10 1) 2))
  (is (= (cut-cards-2 3 10 2) 1))
  (is (cuts-same? 7 10 -5652))
  (is (cuts-same? 7 10 2))
  (is (cuts-same? 7 10 -442))
  (is (cuts-same? 7565 15671 59877))
  (is (cuts-same? 7565 15671 -59877))
  (is (cuts-same? 7565 15671 -59877))
  (is (cuts-same? 13 151 -577))
  (is (cuts-same? 79 151 -5774))
  (is (cuts-same? 15 151 -87977))
  (is (cuts-same? 77 151 7577)))

(defn increment-n-2 [idx count n]
  (mod (* idx n) count))

(defn increment-same? [idx count n]
  (=
   (increment-n-2 idx count n)
   (.indexOf (increment-n (range count) n) idx)))


;; (increment-same? 2 10 7)

(defn symbol->fun-2 [sym]
  (cond
    (= sym :cut) cut-cards-2
    (= sym :new-stack) deal-new-stack-2
    (= sym :deal) increment-n-2))

(def input-2 (reverse (parse-input input)))
(def one-tenth (long (/ 101741582076661 10)))

(def result-2
  (loop [idx 2020
         repetitions 1] ;; use repetitions 101741582076661 numer and this will never finish
    (if (= repetitions 0) idx
        (do
          (when (or
                 (= repetitions one-tenth)
                 (= repetitions (* 2 one-tenth))
                 (= repetitions (* 3 one-tenth))
                 (= repetitions (* 4 one-tenth))
                 (= repetitions (* 5 one-tenth))
                 (= repetitions (* 6 one-tenth))
                 (= repetitions (* 7 one-tenth))
                 (= repetitions (* 8 one-tenth))
                 (= repetitions (* 9 one-tenth))
                 (= repetitions (* 10 one-tenth)))
            (println "progress" (float (/ repetitions 101741582076661))))
          (recur (reduce (fn [acc [symbol coll]]
                           (apply (symbol->fun-2 symbol) acc 119315717514047 coll))
                         idx
                         input-2) (dec repetitions)))))

 (println result-2))
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

(def result-3
  (loop [idx 2020
         repetitions 1]
    (if (= repetitions 0) idx
        (recur (reduce (fn [acc [symbol coll]]
                         (apply (symbol->fun-3 symbol) acc deck-size coll))
                       idx
                       input-3) (dec repetitions)))))

(println (long result-3))
