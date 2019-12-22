(ns day_10.day10
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            ))
(use 'clojure.test)
(load-file "../util/intcode-computer.clj")
(clojure.core/alias 'ic 'util.intcode_computer)


(defn split-input [s]
  (str/split s #","))


(def input (->> (slurp "input.txt")
                (str/trim)
                (split-input)
                (map #(read-string %))
                (ic/enlarge-buffer 50000)
                (vec)))
(count input)
(first input)


(def world* {:painted []}


(do
  (println "================== starting ===================")
  (drop 1 (ic/chomp-input input 0 0 [0 0 0])))
