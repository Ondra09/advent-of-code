#!/usr/local/bin/bb

;; https://adventofcode.com/2020/day/2

(defn split-input [s]
  (str/split s #"\n\n"))

(defn split-pass [s]
  (str/split s #"\n|\ "))

(defn modify-input [col]
  (reduce (fn [acc word]
            (let [[key val] (str/split word #":")]
             (assoc acc key val )))
          {}
          col))

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (map #(split-pass %))
                (map #(modify-input %))))

(defn has-valid [col key fun]
  (map #(if (and (contains? % key)
                 (fun (get % key))) 1 0) col))

(def validation-set (hash-map "byr" #((println %) true)
                     "iyr" #((println %) true)
                     "eyr" #((println %) true)
                     "hgt" #((println %) true)
                     "hcl" #((println %) true)
                     "ecl" #((println %) true)
                     "pid" #((println %) true)))

(def not-required "cid")

(map (fn [[f s]] (println "first: " f "second " s)) validation-set)

;; (def valid-set-marked (map #(has-valid input (first %) (second %)) validation-set))

;; (defn sum-lists [first second]
;;   (map + first second))

;; (def valid-pass (apply + (reduce (fn [acc col]
;;                                    (map #(if (= % 2) 1 0) (sum-lists acc col)))
;;                                  (first valid-set-marked)
;;                                  (rest valid-set-marked))))

;; (println "Part 1 result: " valid-pass)
