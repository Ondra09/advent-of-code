#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))
;; https://adventofcode.com/2020/day/8

(defn split-input [s]
  (str/split s #"\n\n"))


(defn parse-interval [line]
  (let [[_ ll lh hl hh] (re-matches #".*: (\d+)-(\d+) or (\d+)-(\d+)" line)]
    [[(read-string ll) (read-string lh)] [(read-string hl) (read-string hh)]]))

(defn parse-header [line]
  (let [parts (str/split line #"\n")]
    (into {} (map
              #(cond
                     (str/starts-with? % "departure location:") {:dep-loc (parse-interval %)}
                     (str/starts-with? % "departure station:") {:dep-stat (parse-interval %)}
                     (str/starts-with? % "departure platform:") {:dep-plat (parse-interval %)}
                     (str/starts-with? % "departure track:") {:dep-tr (parse-interval %)}
                     (str/starts-with? % "departure date:") {:dep-date (parse-interval %)}
                     (str/starts-with? % "departure time:") {:dep-time (parse-interval %)}
                     (str/starts-with? % "arrival location:") {:arr-loc (parse-interval %)}
                     (str/starts-with? % "arrival station:") {:arr-stat (parse-interval %)}
                     (str/starts-with? % "arrival platform:") {:arr-plat (parse-interval %)}
                     (str/starts-with? % "arrival track:") {:arr-trac (parse-interval %)}
                     (str/starts-with? % "class:") {:class (parse-interval %)}
                     (str/starts-with? % "duration:") {:duration (parse-interval %)}
                     (str/starts-with? % "price:") {:price (parse-interval %)}
                     (str/starts-with? % "route:") {:route (parse-interval %)}
                     (str/starts-with? % "row:") {:row (parse-interval %)}
                     (str/starts-with? % "seat:") {:seat (parse-interval %)}
                     (str/starts-with? % "train:") {:train (parse-interval %)}
                     (str/starts-with? % "type:") {:type (parse-interval %)}
                     (str/starts-with? % "wagon:") {:wagon (parse-interval %)}
                     (str/starts-with? % "zone:") {:zone (parse-interval %)}
                     :else "line") parts))))

(defn parse-ticket [line]
  (let [[_ & tickets] (str/split line #"[\n]")]
    (mapv
     #(mapv read-string (str/split % #",")) tickets)))

(defn parse-input [line]
  (cond (str/starts-with? line "your ticket") {:your-ticket (parse-ticket line)}
        (str/starts-with? line "nearby tickets") {:nearby-tickets (parse-ticket line)}
        :else (parse-header line)))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))
                (into {})))

(defn valid-value? [ranges value]
  (some (fn [[l h]] (and (>= value l) (<= value h)))  ranges))

(defn validate-ticket [ranges values]
  (reduce #(if (not (valid-value? ranges %2)) (conj %1 %2) %1) [] values))

(defn validate-tickets [ranges values]
  (map #(validate-ticket ranges %) values))

(defn remove-invalid [ranges values]
  (filter #(empty? (validate-ticket ranges %)) values))

(def ranges-keys (remove #(or (= % :nearby-tickets) (= % :your-ticket)) (keys input)))

(def valid-only-input (assoc input :nearby-tickets (remove-invalid (apply concat (map input ranges-keys)) (input :nearby-tickets))))

(def columns-count (count (first (valid-only-input :nearby-tickets))))
(def all-tickets (concat (valid-only-input :nearby-tickets) (valid-only-input :your-ticket)))


(def result-list-of-possibilities
  "for every column we find a set of possible candidates"
  (map (fn [col-idx]
         [col-idx (into #{}(remove nil? (map
                                (fn [key] (when (every? true? (map #(valid-value? (valid-only-input key) (nth % col-idx)) all-tickets)) key))
                                ranges-keys)))])
       (range columns-count)))

(defn filter-columns-keys [input]
  (let [sorted (sort-by #(count(second %)) input)]
    (loop [items sorted
           visited #{}
           result []]
      (if (empty? items) result
          (let [[idx possibilities] (first items)
                new-poss (clojure.set/difference possibilities visited)]
            (recur (rest items) (conj visited (first new-poss)) (conj result [idx new-poss])))
          ))))


(def result-mapping
  (let [result-list (filter-columns-keys result-list-of-possibilities)]
    (reduce
     (fn [accum [idx set]] (assoc accum (first set) idx))
     {}
     result-list)
    ))

(def departure-keys [:dep-date :dep-loc :dep-plat :dep-stat :dep-time :dep-tr])
(def departure-indexes (map result-mapping departure-keys))

(println "Part 1 result: " (apply + (flatten (validate-tickets (apply concat (map input ranges-keys)) (input :nearby-tickets)))))
(println "Part 2 result: " (apply * (map #(nth (first (input :your-ticket)) %) departure-indexes)))
;; Part 2 result:  2628667251989
;;;;;;;;;;;;;;;;;;;;;; TESTS
;; (deftest is-valid?
;;   (is (= 5 5)))

;; (defn run-tests []
;;   (is-valid?))

; (run-tests)
