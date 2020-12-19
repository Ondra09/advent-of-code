#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            ;[instaparse.core :as insta]
            ))

;; https://adventofcode.com/2020/day/19

(defn split-input [s]
  (str/split s #"\n\n"))

(defn parse-input [s]
  (str/split s #"\n"))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                ))

(defn parse-automat [input]
  (let [[_ key val] (re-matches #"(\d+)\: (.*)" input)]
    [(read-string key) (vec (read-string (str "(" val ")" )))]))

;; We just stop recursion at certain number, input has limited lenght
(defn expand-re [automat start final]
    (reduce (fn [acc item]
              (let [val (automat item)]
                (cond (nil? val) (conj acc item)
                      (string? (first val)) (conj acc (first val))
                      (true? final) acc;; we do not expand anymore, just delete
                      :else (into [] (concat acc "(" val ")"))))
              ) [] start))


(defn expand-gramatics [automat max-iterations]
  (str "^" (apply str (loop [expansion (expand-re automat [0] false)
                             previous []
                             it 0]
                        (if (or (= previous expansion) (= it max-iterations))
                          expansion
                          (recur (expand-re automat expansion (= (inc it) max-iterations) )
                                 expansion
                                 (inc it))))) "$"))

(def automat (into {} (map parse-automat (first input))))

(defn number-of-matches [re input]
  (reduce
   #(if (re-matches re %2) (inc %1) %1)
   0 input))

(def updated-automat (-> automat
                      (assoc 8 [42 "|" 42 8])
                      (assoc 11 [42 31 (read-string "|") 42 11 31]) ;; this is a^n b^n this is not regular, cant be matched by + or -
                      ))

(println "Part 1 result: " (number-of-matches (re-pattern (expand-gramatics automat 10)) (second input)))
(println "Part 2 result: " (number-of-matches (re-pattern (expand-gramatics updated-automat 14)) (second input)))


;; 301 too low
;;
;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
