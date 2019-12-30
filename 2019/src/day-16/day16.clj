(ns day_16.day16
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            ))
(load-file "../util/simple2D.clj")
(clojure.core/alias 's2D 'util.simple2D)


(use 'clojure.test)

(defn split-input [s]
  (str/split s #""))

(def input (->> (slurp "input.txt")
                (str/trim)
                (split-input)
                (map #(Integer/parseInt %))
                (vec)))
(count input)

(def base-pattern [0 1 0 -1])
(def base-pattern-count (count base-pattern))

(defn get-filter-response [phase idx]
  (let [a-idx (mod (quot (inc idx) phase) base-pattern-count)]
    (nth base-pattern a-idx)))

(deftest response-test
  (is (= (get-filter-response 1 0) 1))
  (is (= (get-filter-response 1 1) 0))
  (is (= (get-filter-response 1 2) -1))
  (is (= (get-filter-response 1 3) 0))
  (is (= (get-filter-response 1 4) 1))
  (is (= (get-filter-response 1 5) 0))
  (is (= (get-filter-response 1 6) -1))
  (is (= (get-filter-response 1 7) 0))

  (is (= (get-filter-response 2 0) 0))
  (is (= (get-filter-response 2 1) 1))
  (is (= (get-filter-response 2 2) 1))
  (is (= (get-filter-response 2 3) 0))
  (is (= (get-filter-response 2 4) 0))
  (is (= (get-filter-response 2 5) -1))
  (is (= (get-filter-response 2 6) -1))
  (is (= (get-filter-response 2 7) 0))

  (is (= (get-filter-response 3 0) 0))
  (is (= (get-filter-response 3 1) 0))
  (is (= (get-filter-response 3 2) 1))
  (is (= (get-filter-response 3 3) 1))
  (is (= (get-filter-response 3 4) 1))
  (is (= (get-filter-response 3 5) 0))
  (is (= (get-filter-response 3 6) 0))
  (is (= (get-filter-response 3 7) 0)))

(get-filter-response 2 3)

(def test-input-1 [1 2 3 4 5 6 7 8])
(def test-input-2 (->> "80871224585914546619083218645595"
                       split-input
                       (map #(read-string %))))

(defn right-most-digit [num]
       (mod (s2D/abs num) 10))

(defn multiply-input [input phase]
  (right-most-digit (second (reduce
                     (fn [[idx sum] num] [(inc idx) (+ sum (* num (get-filter-response phase idx)))])
                     [0 0]
                     input))))

;;(multiply-input test-input-1 1)

(defn apply-input [input-count input]

    (for [i (range 1 (inc input-count))] ;; for loop!
      (multiply-input input i)))

(time (take 8 (apply-input (count input) input)))
(* 100 0.04422)

(defn apply-input-n-times [input n]
  (let [input-count (count input)]
    (loop [m-input input
           new-n n]
      (if (= new-n 0)
        m-input
        (recur (apply-input input-count m-input) (dec new-n))
        ))))

(time
 (println "Part 1: " (take 8 (apply-input-n-times input 100))))

;; Part 1:  (2 7 2 2 9 2 6 9)
;; "Elapsed time: 2734.213321 msecs"

;;;;;;;;;;;;;;;;;;;;; Part 2 ;;;;;;;;;;;;;;;;;;;;;;;;
;; ok so this is basically cosine transform
;; phase is 1/phase frequency of cosine wave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; pattern is like this:
  ; 1 0 -1 0 1  0 -1
  ; 0 1  1 0 0 -1 -1
  ; 0 0  1 1 1  0  0
  ; 0 0  0 1 1  1  1
  ; 0 0  0 0 1  1  1
  ; 0 0  0 0 0  1  1
  ; 0 0  0 0 0  0  1

  ; That means that for position n we do need to compute all < n positions.
  ; Becasue they will be 0, also fro N > (count input) /2 there are no -1
  ; and because our offset is really large, we can skip majority of input
; also, we should sum it from back so we can reuse all previous sums

(def offset (read-string (subs input 0 7)))

(def input-part-2 (drop offset (flatten (repeat 10000 input))))
(count input-part-2)

(defn sum-line [input]
  (loop [cos-trans (transient (vec input))
         n (dec (count input))]
    (if (= n 0)
      (persistent! cos-trans)
      (recur (assoc! cos-trans
                     (dec n)
                     (mod (+ (nth cos-trans n) ;; we need only one digit so mod 10
                             (nth cos-trans (dec n))) 10))
             (dec n)))))

(defn sum-hundred-times [input]
  (loop [n 100
         input input]
    (if (= n 0)
      (take 8 input)
      (recur (dec n) (sum-line input))
    )))
(time
 (println "Phase 2: " (sum-hundred-times input-part-2)))

;; (2 6 8 5 7 1 6 4)
;; "Elapsed time: 7330.081778 msecs"
