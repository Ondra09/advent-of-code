#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]))

;; https://adventofcode.com/2020/day/21

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (let [[_ ingredients alergens] (re-matches #"([\w ]+) \(contains ([\w\, ]+)\)" line)]
    {:ingredients (into #{} (str/split ingredients #" "))
     :alergens (into #{} (str/split alergens #", "))}))


(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (mapv #(parse-input %))))

(def alergen-map (reduce
                  (fn [acc {:keys [ingredients alergens]}]
                    (reduce
                     (fn [acc alergen]
                       (assoc acc alergen (conj (acc alergen) ingredients)))
                     acc alergens))
                  {} input))


(def alergen-to-ingredience (into {} (map
                                      (fn [[name recipes]]
                                        [name (reduce (fn [acc recipe] (clojure.set/intersection acc recipe))  recipes)])
                                      alergen-map)))

(defn reduce-alergen-ingrediences [mapping]
  (loop [alergens mapping]
    (let [visited (reduce #(clojure.set/union %1 (second %2)) #{} (filter (comp #(= 1 %) count second)  alergens))
          not-visited (filter (comp #(> % 1) count second) alergens)
          ]
      (if (empty? not-visited)
        alergens
        (recur (reduce
                (fn [reduced-alergens [name alergens]]
                  (if (> (count alergens) 1)
                    (assoc reduced-alergens name (clojure.set/difference alergens visited))
                    (assoc reduced-alergens name alergens)
                    ))
                {}
                alergens))))))

(def bad-ingrediences  (reduce #(clojure.set/union %1 (second %2)) #{} (reduce-alergen-ingrediences alergen-to-ingredience)))

(println "Part 1 result: " (count (filter #(not (contains? bad-ingrediences %1)) (reduce #(concat %1 (%2 :ingredients)) [] input))))
(println "Part 2 result: " (str/join "," (reduce #(concat %1 (second %2))  [] (sort-by first (reduce-alergen-ingrediences alergen-to-ingredience)))))
