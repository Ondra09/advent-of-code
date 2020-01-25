(ns day_19.day19
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

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

(time
 (println "============= Part 1: =============\n"
          (reduce + (for [i (range 50)
                          j (range 50)]
                      (first (ic/out-params (ic/chomp-input input 0 0 `(~i ~j))))))))
;; ============= Part 1: =============
;; 197
;; "Elapsed time: 3423.65868 msecs"

;; =============================================================================
;; ============================= Part 2: =======================================
;; =============================================================================
;; approach is to find point on line 1 that holds [+10 -10] for point on line 2
;; +------------------>x
;; |X
;; | XX
;; |   XXX
;; |    XXXXXX    p1
;; |        XXXXXXX
;; |          p0XXXXXX
;; v
;; y
;; p1 = p0 + {:x 100 :y -100}
;; a1x + b1y + c  = 0  ;; c is 0 because it goes through origin
;;
;; a0x + b0x = 0
;; a1x + b1x = 0 ;; after simplification we have compute-part-2

(defn compute-part-2 [P0 P1 width]
  (let [line0 (s2D/create-line-equation {:x 0 :y 0} P0)
        line1 (s2D/create-line-equation {:x 0 :y 0} P1)
        a0b0 (/ (:a line0)  (:b line0))
        a1b1 (/ (:a line1) (:b line1))
        x0 (/ (- width (* width a1b1))
              (- a1b1 a0b0))
        y0 (* -1.0 (* a0b0 x0))
        x1 (+ x0 width)
        y1 (- y0 width)

        ff (println a0b0)
        ff (println a1b1)]
    [[(Math/round x0) (Math/round y0)]
     [(Math/round x1) (Math/round y1)]]
    [[x0 y0]
     [x1 y1]]))


;; slope of second line is very close to 1, we are dividing almost the same number, so we need to have high precision
(def line-to-search 5000)

(def p0 {:x
         (loop [i 2000]
           (let [on-line (first (ic/out-params (ic/chomp-input input 0 0 `(~i ~line-to-search))))]
             (if (or (= on-line 1))
               i
               (recur (inc i))))) :y line-to-search})
(def p1 {:x line-to-search :y (loop [i 3000]
                                (let [on-line (first (ic/out-params (ic/chomp-input input 0 0 `(~line-to-search ~i))))]
                                  (if (or (= on-line 1))
                                    i
                                    (recur (inc i)))))})

(defn coords->final-result [[[x0 y0] [x1 y1]]]
  [x0 y1])

;; after rounding
;; 918 1022
(println "Result 2: " (coords->final-result (compute-part-2 p0 p1 99.0)))

;; #.......................................
;; .#......................................
;; ..##....................................
;; ...###..................................
;; ....###.................................
;; .....####...............................
;; ......#####.............................
;; ......######............................
;; .......#######..........................
;; ........########........................
;; .........#########......................
;; ..........#########.....................
;; ...........##########...................
;; ...........############.................
;; ............############................
;; .............#############..............
;; ..............##############............
;; ...............###############..........
;; ................###############.........
;; ................#################.......
;; .................########OOOOOOOOOO.....  25, 20
;; ..................#######OOOOOOOOOO#....
;; ...................######OOOOOOOOOO###..
;; ....................#####OOOOOOOOOO#####  39, 23
;; .....................####OOOOOOOOOO#####
;; .....................####OOOOOOOOOO#####
;; ......................###OOOOOOOOOO#####
;; .......................##OOOOOOOOOO#####
;; ........................#OOOOOOOOOO#####
;; .........................OOOOOOOOOO#####
;; ..........................##############
;; ..........................##############
;; ...........................#############
;; ............................############
;; .............................###########

;; x,y
;; 29,34

;; test
;; (compute-part-2 {:x 29 :y 34} {:x 39 :y 23} 9.0)
;; (compute-part-2 {:x 25 :y 29} {:x 34 :y 20} 9.0)
