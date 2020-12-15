#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/13

(defn split-input [s]
  (str/split s #"\n"))

(defn num->binary-str [input]
  (Integer/toString input 2))

(defn binary-str->num [input]
  (read-string (str "2r" input)))

(defn parse-mask [line]
  (let [[_ match] (re-matches #"mask = ([01X]+)" line)]
  {:type :mask
   :val match}))

(defn parse-mem [line]
  (let [[_ idx val] (re-matches #"mem\[(\d+)\] = (\d+)" line)]
    {:type :mem
     :val {:idx (num->binary-str (read-string idx))
           :val (num->binary-str (read-string val))}}))

(defn parse-input [line]
  (cond (str/starts-with? line "mem") (parse-mem line)
        (str/starts-with? line "mask") (parse-mask line)))

(def bits 36)

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                ))

(defn fill-zeros [n count]
  (str (apply str (repeat count \0)) n))

(defn mask-number [target mask]
  (apply str (reverse
              (mapv
               (fn [t m] (if (not= m \X) m t))
               (reverse (fill-zeros target (- (count mask) (count target))))
               (reverse mask)))))

(defn mask-number-pt2 [target mask]
  (apply str (reverse
              (mapv
               (fn [t m] (if (not= m \0) m t))
               (reverse (fill-zeros target (- (count mask) (count target))))
               (reverse mask)))))

(defn first-or-zero [s]
  (let [val (first s)]
    (if (nil? val) \0 val)))

(defn replace-eXs [mask val]
  (let [binval (num->binary-str val)]
    (first (reduce
            (fn [[result binval] c]
              (if (= c \X) [(str result (first-or-zero binval)) (rest binval)]
                  [(str result c) binval]))
            ["" (reverse binval)]
            mask))))

(defn generate-floating-combinations [mask]
  (let [freq (frequencies mask)]
    (map
     (fn [variant] (replace-eXs mask variant))
     (range (Math/pow 2 (freq \X))))))

(defn compute-part-1 [input]
  (reduce (fn [{:keys [mem m0]} {:keys [type val]}]
            (cond (= type :mask) {:mem mem :m0 val}
                  (= type :mem) {:mem (assoc mem (val :idx) (mask-number (val :val) m0)) :m0 m0}))
          {:mem {}
           :m0 ""}
          input))

(defn compute-part-2 [input]
  (reduce (fn [{:keys [mem m0]} {:keys [type val]}]
            (cond (= type :mask) {:mem mem :m0 val}
                  (= type :mem) {:mem (reduce #(assoc %1 %2 (val :val)) mem (generate-floating-combinations (mask-number-pt2 (val :idx) m0)))
                                 :m0 m0}))
          {:mem {}
           :m0 ""}
          input))

(defn sum-mem [mem]
  (reduce (fn [accum [k v]] (+ accum (binary-str->num v))) 0 mem))

(println "Part 1 result: " (sum-mem ((compute-part-1 input) :mem)))
(println "Part 2 result: " (sum-mem ((compute-part-2 input) :mem)))

;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))
