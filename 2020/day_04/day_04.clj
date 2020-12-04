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

(defn validate-number [n min max]
  (and (>= n min)
       (<= n max)))

(defn validate-height [height min-cm max-cm min-in max-in]
  (let [[val units] [(subs height 0 (- (count height) 2))
                      (subs height (- (count height) 2))]]
    (cond (= units "cm") (validate-number (Integer/parseInt val) min-cm max-cm)
          (= units "in") (validate-number (Integer/parseInt val) min-in max-in)
          :else false)))

(defn validate-color [c]
  (not (nil? (re-matches #"#[0-9a-f]{6}" c))))

(def eye-tbl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(defn validate-eye [e]
  (contains? eye-tbl e))

(defn validate-pass-id [pass]
  (not (nil? (re-matches #"[0-9]{9}" pass))))

(def validation-set (hash-map
                     "byr" (fn [w] (validate-number (Integer/parseInt w) 1920 2002))
                     "iyr" (fn [w] (validate-number (Integer/parseInt w) 2010 2020))
                     "eyr" (fn [w] (validate-number (Integer/parseInt w) 2020 2030))
                     "hgt" (fn [w] (validate-height w 150 193 59 76))
                     "hcl" (fn [w] (validate-color w))
                     "ecl" (fn [w] (validate-eye w))
                     "pid" (fn [w] (validate-pass-id w))))

(def not-required "cid")

;; (map (fn [[f s]] (println "first: " f "second " s) "a") validation-set)

(def valid-set-marked
  (map (fn [[k v]] (has-valid input k v)) validation-set))

(defn sum-lists [first second]
  (map + first second))

(def valid-pass (apply + (reduce (fn [acc col]
                                   (map #(if (= % 2) 1 0) (sum-lists acc col)))
                                 (first valid-set-marked)
                                 (rest valid-set-marked))))

;; after changes to part 2; the Part 1 validation is too strict (remove validation functions)
(println "Part 2 result: " valid-pass)
