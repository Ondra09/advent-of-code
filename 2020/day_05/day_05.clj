#!/usr/local/bin/bb

;; https://adventofcode.com/2020/day/5

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (let [matches (re-matches #"([FB]+)([LR]+)" line)]
    [(nth matches 1) (nth matches 2)]))


(defn modify-input [col]
  [(seq (char-array (first col)))
   (seq (char-array (second col)))])

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                (map #(modify-input %))))


(defn create-result [row col]
  (+ (* row 8) col))

(defn middle-val [low high]
  (/ (+ low high) 2))

(defn decode-one [char [low-val high-val]]
  (cond (= char \F) [low-val (middle-val low-val high-val)]
        (= char \B) [(middle-val low-val high-val) high-val]
        (= char \L) [low-val (middle-val low-val high-val)]
        (= char \R) [(middle-val low-val high-val) high-val]))


(defn decode-word [[word-row word-col]]
  (create-result (first (reduce (fn [accum char] (decode-one char accum)) [0 128] word-row))
                 (first (reduce (fn [accum char] (decode-one char accum)) [0 8] word-col))))

(def seat-ids (map #(decode-word %) input))

(println "Part 1 result: "
         (apply max seat-ids))

;; this is not functional at all
(defn find-missing-in-row [col]
  (reduce (fn [[acc result] val]
            (if (= (inc acc) val)
              [val result]
              [val (dec val)]))
          [(first col) 0] (rest col)))

(println "Part 2 result, Missing value: " (second (find-missing-in-row (sort seat-ids))))
