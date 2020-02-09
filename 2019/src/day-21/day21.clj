(ns day_21.day21
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(use 'clojure.test)

(load-file "../util/intcode-computer.clj")
(clojure.core/alias 'ic 'util.intcode_computer)
(load-file "../util/simple2D.clj")
(clojure.core/alias 's2D 'util.simple2D)


(defn split-input [s]
  (str/split s #","))

(def input (->> (slurp "input.txt")
                (str/trim)
                (split-input)
                (map #(read-string %))
                (ic/enlarge-buffer 10000)
                (vec)))

(def PROGRAM ["NOT A J"
              "NOT B T"
              "OR T J "
              "NOT C T"
              "OR T J"
              "AND D J"
              "WALK"])


(def complete-input
  (->> PROGRAM
       (map #(ic/ascii->intcode %))))

(do
  (println  "Result 1:")
  (let [output (ic/out-params (ic/chomp-input input 0 0 (flatten complete-input)))]
    (do
      (println (ic/intcode->ascii (drop-last output)))
      (println "Last: " (last output)))))

;; ============================= Part 2
(def PROGRAM-2 ["NOT A J"
                "NOT B T"
                "OR T J "
                "NOT C T"
                "OR T J"
                "AND D J" ;; same until now
                "NOT H T" ;; try and error solution
                "NOT T T"
                "AND T J"
                "NOT A T"
                "OR T J"
                "RUN"])

(do
  (println  "Result 2:")
  (let [complete-input (->> PROGRAM-2
                            (map #(ic/ascii->intcode %)))
        output (ic/out-params (ic/chomp-input input 0 0 (flatten complete-input)))]
    (do
      (println (ic/intcode->ascii (drop-last output)))
      (println "Last: " (last output)))))
