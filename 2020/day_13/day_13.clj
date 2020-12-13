#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/8

(defn split-input
  ([s] (str/split s #"\n"))
  ([delimiter s] (str/split s delimiter)))



(defn parse-input [[time buses]]
  {:time (read-string time)
   :buses (->> buses
               (split-input #"\,")
               (mapv #(read-string %)))
               })

(def input-raw (->> (slurp "input")
                    (split-input)
                    (vec)))

(def input (parse-input input-raw))

;; https://rosettacode.org/wiki/Modular_inverse#Clojure
(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(Math/abs b) 0 1]
        (zero? b) [(Math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (Math/abs b)
                     r0 (Math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))
;; https://rosettacode.org/wiki/Chinese_remainder_theorem#Clojure
(defn chinese-remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
                                                      ; (map vector n a) is same as
        ;                                             ; Python's version Zip (n, a)
    (mod sum-prod prod)))                             ; Result line



(defn compute-next-departure [time bus]
  (+ bus (- time (mod time bus))))

(defn sort-by-nearest-time [input]
  (->> (input :buses)
       (filter number?)
       (sort-by (partial compute-next-departure (input :time)))))

(def part-1
  (let [next-bus (first (sort-by-nearest-time input))]
    (* next-bus (- next-bus (mod (input :time) next-bus)))))


(println "Part 1 result: " part-1)

(def bus-idx (reduce (fn [[val-coll remainder-coll] bus]
                       (if (number? bus)
                         [(conj val-coll bus) (conj remainder-coll (- bus (.indexOf (input :buses) bus)))]
                         [val-coll remainder-coll]))
                     [[][]] (input :buses)))


; (println "Test 1 (3417): " (chinese-remainder [17 13 19] [0  11 16]))
; (println "Test 2 (1202161486): " (chinese-remainder [1789 37 47 1889] [0  36 45 1886]))

;; not right 599534645305388
;; correct 305068317272992
(println "Part 2 result: " (apply chinese-remainder bus-idx))

;; implementation of chinese theorem from roseta code is not working correclty
;; so I put numbers and remaindes from bus-idx list by hand to form on this
;; page https://www.dcode.fr/chinese-remainder

;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
