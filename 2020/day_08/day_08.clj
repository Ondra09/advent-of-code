#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/8

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (let [[_ instruction num] (re-matches #"(\w+)\ ([\+-]\d+)" line)]
    [instruction (read-string num)]))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                (vec)))

(def machine-state {:ip 0
                    :acc 0
                    :prog input})

(defn incrase-state [state key amount]
  (assoc state key (+ amount (state key))))

(def instruction-table {"nop" (fn [state amount] (incrase-state state :ip 1)),
                        "acc" (fn [state amount] (incrase-state (incrase-state state :acc amount) :ip 1)),
                        "jmp" (fn [state amount] (incrase-state state :ip amount))
                        "XXX" (fn [state amount] state)})

(defn invalidate-program [program line]
  (assoc program line ["XXX" 0]))

(defn invalidate-program-state [state line]
  (assoc state :prog (invalidate-program (state :prog) line)))

(defn simulate [state]
  (let [[inst val] (nth (state :prog) (state :ip))
        new-state (-> state
                      ((instruction-table inst) val)
                      (invalidate-program-state (state :ip))
                      )]
    (if (or (= "XXX" inst)
            (= (new-state :ip) (count (state :prog))))
      new-state
      (simulate new-state)
      )))


(defn flip-nop-jmp [prog line]
  (let [[in val] (nth prog line)]
    (cond (= in "nop") (assoc prog line ["jmp" val])
          (= in "jmp") (assoc prog line ["nop" val])
          :else prog)))

(defn flip-state [state line]
  (assoc state :prog (flip-nop-jmp (state :prog) line)))

(println "Part 1 result: " ((simulate machine-state) :acc))

(def all-programs (map (fn [line] (flip-state machine-state line)) (range (count input))))

(println "Part 2 result: " ((some (fn [state] (when (= (state :ip) (count input)) state)) (map #(simulate %) all-programs)) :acc))

;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
