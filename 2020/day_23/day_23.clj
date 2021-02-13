#!/usr/local/bin/bb

(ns main
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            ))

;; https://adventofcode.com/2020/day/23
(def input-test "389125467")

(def input "872495136")


(defn pick-one [input last-idx]
  (nth input (mod (inc last-idx) (count input))))


(defn three-substr [input idx]
  (subs input idx (+ idx 3)))
