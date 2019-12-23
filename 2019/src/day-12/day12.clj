(ns day_12.day12
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            ))
(use 'clojure.test)

(load-file "../util/simple2D.clj")
(clojure.core/alias 's2D 'util.simple2D)


(def input-pos [[16 -8 13]
                 [4 10 10]
                 [17 -5, 6]
                 [13 -3, 0]])

(def input-vel (vec (repeat 4 [0 0 0])))

(defn gen-all-tuples [n]
  (for [x (range 0 n)
        y (range (inc x) n)]
    [x y]))

(defn compute-delta-velocity-for-coord [coord0 coord1]
  (cond
    (> coord0 coord1) [-1 1]
    (< coord0 coord1) [1 -1]
    :else [0 0]
    ))

(defn update-vectors [vec0 vec1]
  (apply map
         (fn [x y z]
           [x y z])
         (map
          (fn [x0 x1]
            (compute-delta-velocity-for-coord x0 x1))
          vec0 vec1)))

(defn sum-vector [vec0 vec1]
  (map +
   vec0
   vec1))

(defn update-velocities [velocities positions]
  (reduce
   (fn [input [one second]]
     (let [vec0 (nth positions one)
           vec1 (nth positions second)
           [vel0-out vel1-out] (update-vectors vec0 vec1)
           vel0 (nth input one)
           vel1 (nth input second)
           input (assoc input one (sum-vector vel0 vel0-out))
           input (assoc input second (sum-vector vel1 vel1-out))]
       input))
   velocities
   (gen-all-tuples 4))) ;; TODO: this is hard coded

(defn update-positions [velocities positions]
  (map (fn [vec0 vec1] (sum-vector vec0 vec1)) velocities positions))

(defn test-step [velocities positions]
  (let [new-velocities (update-velocities velocities positions)]
  [new-velocities (update-positions new-velocities  positions)]))

(defn run-simulation-for-steps [steps velocities positions]
  (loop [n steps
         [velocities positions] [velocities positions]]
    (if (= n 0)
      [velocities positions]
        (recur (dec n) (test-step  velocities positions)))))

(test-step input-vel input-pos)

(defn compute-energy [vec]
  (reduce #(+ %1 (s2D/abs %2)) 0 vec))

;; part 1
(def after-1000-steps (run-simulation-for-steps 1000 input-vel input-pos))

(defn compute-energy-lst [lst]
  (map
   (fn [vec]
     (compute-energy vec))
   lst))

(println "Result: "(apply +
                          (map
                           #(* %1 %2)
                           (compute-energy-lst (first after-1000-steps))
                           (compute-energy-lst (second after-1000-steps)))))

;; part 2
;; run until same as beginning

(defn simulate-until-same [velocities positions]
  (loop [[velos pos] (test-step velocities positions)
         n 1]
    (if (or (= n 0)(= velocities velos))
      (do
        (* 2 n))
      (do
        (when (= (mod n 50000) 0)
          (println n))
        (recur (test-step velos pos) (inc n))))))

;; should stop after 2772 steps
(def test-input [[-1 0 2] [2 -10 -7] [4 -8 8] [3 5 -1]])
(simulate-until-same input-vel test-input)

;; works only for 4 moons
(defn compute-each-band-separately [positions]
  (for [idx (range 0 3)]
    (simulate-until-same
     input-vel
     (map (fn [[x y z]]
            (cond
              (= idx 0) [x 0 0]
              (= idx 1) [0 y 0]
              (= idx 2) [0 0 z]))
          positions)
     )))

(compute-each-band-separately test-input)

;; should give 2772
(s2D/lcmv 18 28 44)

(def periods (compute-each-band-separately input-pos))

(println "Result 2: " (apply s2D/lcmv periods))
