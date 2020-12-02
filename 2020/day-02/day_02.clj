#!/usr/local/bin/bb

;; https://adventofcode.com/2020/day/2

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (re-matches #"(\d+)\-(\d+) (\w)\: (\w+)" line))

(defn get-low [col]
  (Integer/parseInt(nth col 1)))

(defn get-high [col]
  (Integer/parseInt(nth col 2)))

(defn get-char [col]
  (char (first (.getBytes (nth col 3)))))

(defn get-password [col]
  (nth col 4))

(defn modify-input [col]
  [(get-low col) (get-high col) (get-char col) (get-password col)])

(def input (->> (slurp "input")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                (map #(modify-input %))))



(defn validate-password [lower upper char password]
  (let [freq (frequencies password)]
    (and
     (not (nil? (get freq char)))
     (>= (get freq char) lower)
     (<= (get freq char) upper))))

(def valid-passwords (filter #(apply validate-password %) input))

(println "Number of valid passwords: " (count valid-passwords))

(defn korp-get [col idx]
  (get col (dec idx)))

(defn bool-to-int [val]
  (get { false 0 true 1 } val))

(defn validate-password-2 [lower upper char password]
  (= 1 (+ (bool-to-int (= char (korp-get password lower)))
          (bool-to-int (= char (korp-get password upper))))))

(def valid-passwords-2 (filter #(apply validate-password-2 %) input))

(println "Number of valid passwords: " (count valid-passwords-2))
