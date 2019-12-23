(ns day_11.day11
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


(def world* {:painted []})

(defn decode-direction [turn-direction]
  (if (= turn-direction 0) :left :right))

(defn decode-color [color]
  (if (= color 0) \B  \W))

(defn decode-output [color turn-direction]
  [(decode-color color)
   (decode-direction turn-direction)])

;; this holds for right-hand coord system
(defn rotate-left [x y]
  [(* -1 y) x])

(defn rotate-right [x y]
  [y (* -1 x)])

(defn move
  "return new-position new-forward"
  [[x y] forward turn-direction]
  (let [new-forward (if (= turn-direction :left)
                      (apply rotate-left forward)
                      (apply rotate-right forward))]
    [[(+ x (first new-forward)) (+ y (second new-forward))]  new-forward]
    ))

(deftest move-test
  (is (= [-1 0] (first (move [0 0] [0 1] :left))))
  (is (= [-8 -13] (first (move [-8 -12] [-1 0] :left))))
  (is (= [-8 -11] (first (move [-8 -12] [-1 0] :right)))))

(def start-position [0 0])
(def start-forward [0 1])

(defn find-color [list pos]
  (let [item (find list pos)]
    (if item
      (second item)
      \B
      )))

(defn color->input [color]
  (if (= color \B)
    0
    1))

(defn find-color->input [list pos]
  (color->input (find-color list pos)))


(defn start-painting [initial-value]
  (do
    (println "================== starting ===================")
    (loop [ic-output (ic/chomp-input input 0 0 [initial-value])
           position start-position
           forward-dir start-forward
           paint-list {}
           ;; safety check
           iterations 0]
      (let [[color direction] (ic/out-params ic-output)
            [new-color new-direction] (decode-output color direction) ;; eg. \W :left
            [new-pos  new-forward] (move position forward-dir new-direction)
            new-ic-params (ic/assoc-output (ic/out-params->in-params ic-output) (conj [] (find-color->input paint-list new-pos)))
            ]
        (if (or (= iterations 500000)(not= :block (ic/status ic-output)))
          (do
            (println "result: " (ic/status ic-output))
            paint-list)
          (recur (apply ic/chomp-input new-ic-params) new-pos new-forward  (assoc paint-list position new-color) (inc iterations)))))))

;; first part
(println "No of painted panels: " (count (start-painting 0)))


;; second part
(def result-list (start-painting 1))

;; print it
;;[[x y] \W]
(defn complete-line [min max lst]
  (loop [pos min
         new-lst lst]
    (if (= pos max)
      new-lst
      (if (nil? (some #(= pos (first (first %))) new-lst))
          (recur (inc pos) (conj new-lst [[pos 0] \B]))
          (recur (inc pos) new-lst)
      ))))

(do
  (println "")
  (doseq [y  (range  0 -6 -1)] ;; we have positive y upwards, so we need to transform it by X axis
    (->> result-list
         (filter #(= (second (first %)) y)) ;; filter row
         (complete-line -10 51)
         (sort-by (juxt first first))
         (map #(if (= \W (second %)) \X \.)) ;; make it more readable
         (println))
    ))

;; PGUEPLPR
