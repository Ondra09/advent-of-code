#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            ))

;; https://adventofcode.com/2020/day/11

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (->> (str/split line #"\ ")
       (str/join)))


(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                ))

(def precendences-pt1 {\+ 1
                       \* 1
                       \( 0
                       \) 0})

(def precendences-pt2 {\+ 2
                       \* 1
                       \( 0
                       \) 0})

(defn higher-precedence? [a b precedence-table]
  ( > (precedence-table a) (precedence-table b)))

;; Algorithm description https://www.includehelp.com/c/infix-to-postfix-conversion-using-stack-with-c-program.aspx
(defn pop-stack [stack op precedence-table]
  "popping stack when precedenc of current op lower or equal than on top of stack"
  (loop [cur-stack stack
         out []]
    (let [top (first cur-stack)]
      (if (or (nil? top)
              (higher-precedence? op top precedence-table))
        [(conj cur-stack op) out]
        (recur (rest cur-stack) (conj out top))))))

(defn pop-all-until-parent [stack out]
  (loop [cur-stack stack
         out out
         ]
    (if (or (empty? cur-stack) (= (first cur-stack) \())
      [(rest cur-stack) out]
      (recur (rest cur-stack) (into [] (conj out (first cur-stack)))))))

(defn infix->postfix [eq precedence-table]
  (second (reduce
   (fn [[stack out] c]
     (do
       ;;(println stack "  " out "   " c)
       (cond (= c \() [(conj stack c) out]
             (= c \)) (pop-all-until-parent stack out)
             (contains? precendences c) (let [[new-stack stack-out] (pop-stack stack c precedence-table)
                                              new-out (into [](concat out stack-out))]
                                          [new-stack new-out])
             :else [stack (conj out c)])))
   ['(\() []] (str eq \)))))


(defn evaluate-postfix [eq]
  (first (reduce
          (fn [stack c]
            (do ;(println stack c)
              (cond (= c \+) (conj (drop 2 stack) (+ (first stack) (second stack)))
                    (= c \*) (conj (drop 2 stack) (* (first stack) (second stack)))
                    :else (conj stack (Character/digit c 10))
                    )))
          '() eq)))

(defn evaluate [input precendences]
  (reduce
   (fn [acc line]
     (+ acc (evaluate-postfix(infix->postfix line precendences))))
   0
   input))
(println "Part 1 result: " (evaluate input precendences-pt1))
(println "Part 2 result: " (evaluate input precendences-pt2))


;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
