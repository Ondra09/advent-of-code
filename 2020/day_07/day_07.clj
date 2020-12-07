#!/usr/local/bin/bb
(require '(clojure.test :refer [deftest is])
         '(clojure.string :refer [includes?]))
;; https://adventofcode.com/2020/day/7

(defn split-input [s]
  (str/split s #"\n"))

(defn parse-input [line]
  (let [[_ name items] (re-matches #"([ \w]+) bags contain (.*)" line)]
    {name items}))


(defn modify-input [col]
  (into {}[(first col) (rest col)]))

(def input (->> (slurp "input-test")
                (str/trim)
                (split-input)
                (map #(parse-input %))
                ;(map #(modify-input %))
                (into {})
                ))



(defn is-child? [col name]
  (includes? (second col) name))

(defn visited? [v col]
  (some #(= % v) col))

(defn find-all-parents [nodes node-name]
  (loop [nodes nodes
         node-names [node-name]
         visited #{}]
    (if (empty? node-names)
      visited
      (let [parents (filter #(is-child? % (first node-names)) input)
            not-visited (filter #(not (visited? % visited)) parents)]
        (recur nodes not-visited (conj visited node-name))
        ))))

(filter #(is-child? % "shiny gold") input)


;;;;;;;;;;;;;;;;;;;;;; TESTS
(deftest is-valid?
  (is (= 5 5)))

(defn run-tests []
  (is-valid?))

; (run-tests)
; (filter #(is-child? % "shiny gold") input)

; ((hash-map "abc" "aaa") "abc")
; (find-all-parents input "shiny gold")
;
input
