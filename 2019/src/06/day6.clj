(ns advent.day5 ;; this is wrong ns!
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]
            ))

(def world {})

(def input
  (->> "input.txt"
       io/reader
       line-seq
       ;;first
       (format "%s")
       read-string
       ))
(take 5 input)

(def test-input
  '("COM)B"
    "B)C"
    "C)D"
    "D)E"
    "E)F"
    "B)G"
    "G)H"
    "D)I"
    "E)J"
    "J)K"
    "K)L"
    "K)YOU"
    "I)SAN"))

(def split-input (map #(s/split %1 #"\)") input))

(take 5 split-input)

(def split-test-input (map #(s/split %1 #"\)") test-input))
(take 5 split-test-input)

;; (defn create-tree [split-input]
;;   (reduce
;;    (fn [buf [key val]]
;;      (let [values (get buf key)]
;;        (assoc buf key (conj values val))))
;;    {}
;;    split-input))

(defn create-tree [split-input]
  (reduce
   (fn [buf [key val]]
       (assoc buf val key))
   {}
   split-input))

(def test-tree (create-tree split-test-input))

(defn distance-to-root [buf start-key]
  (loop [key start-key
         length 0]
    (if-let [parent (get buf key)]
      (do
        ;;(println "key " key " parent " parent)
        (recur parent (inc length)))
      length)))

(defn sum-all-distances [tree]
  (reduce (fn [sum [key val]]
            (+ sum (distance-to-root tree key))) 0 tree))

(sum-all-distances (create-tree split-input))

(defn path-to-root [buf start-key]
  (loop [key start-key
         path '()]
    (if-let [parent (get buf key)]
      (do
        (recur parent (conj path parent)))
      path)))

;; from "YOU" to "SAN"
(defn get-path-hops-number [tree]
  (let [you-path (set (path-to-root tree "YOU"))
        san-path (set (path-to-root tree "SAN"))]
    (+ (set/difference you-path san-path)
       (set/difference san-path you-path))))

(get-path-hops-number (create-tree split-test-input))


;; (create-tree split-input)
;; split-test-input
;; split-input
