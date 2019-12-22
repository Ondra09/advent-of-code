(ns day_09.day9
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            ))

(defn split-input [s]
  (str/split s #","))

;; enlarge buffer
(defn enlarge-buffer [size buf]
  (concat buf (repeat size 0)))

(def input (->> (slurp "input.txt")
                (str/trim)
                (split-input)
                (map #(Integer/parseInt %))
                (enlarge-buffer 50000)
                (vec)))
(count input)
(first input)
(defn read-value [data mode val relative-base]
  (cond
    (= mode 1) val ;; immediate mode
    (= mode 2) (nth data (+ relative-base val)) ;; relative mode
    :else (nth data val) ;; postion mode 0
    ))

(defn read-output-value [data mode val relative-base]
  (cond
    (= mode 1) val ;; immediate mode
    (= mode 2) (do
                 (+ relative-base val)) ;; relative mode
    :else val ;; postion mode 0
    ))

(defn write-address [data address-to-write value]
  (assoc data value))


(defn get-op [code]
  "returns vector [fun advance-no]"
  (cond
    (= code 1) [(fn [[input sp b-offset input-data output-data] x y target] ;; sum
                  [(assoc input target (+ x y)) (+ sp 4) b-offset input-data output-data :ok]) 2 1] ;; add 2 params, store function
    (= code 2) [(fn [[input sp b-offset input-data output-data] x y target] ;; mul
                  ;;(println "Code2 : " target "  " (nth input target))
                  [(assoc input target (* x y)) (+ sp 4) b-offset input-data output-data :ok]) 2 1] ;; mul 2 params, one store param
    (= code 3) [(fn [[input sp b-offset input-data output-data] target] ;; store
                  ;;(println "storing to: " target)
                  (if (empty? input-data)
                    [input sp b-offset input-data output-data :block]
                    [(assoc input target (first input-data)) (+ sp 2) b-offset (drop 1 input-data) output-data :ok])) 0 1] ;; 0 read param; 1 store
    (= code 4) [(fn [[input sp b-offset input-data output-data] x] ;; output value
                  ;;(println "outputting: " x)
                  [input (+ sp 2) b-offset input-data (conj output-data x) :ok]) 1 0] ;; 1 read 0 output
    (= code 5) [(fn [[input sp b-offset input-data output-data] x target]
                  (if (> x 0)
                    [input target b-offset input-data output-data :ok]
                    [input (+ sp 3) b-offset input-data output-data :ok])) 2 0] ;; jump if true; 2 read params 0 store param
    (= code 6) [(fn [[input sp b-offset input-data output-data] x target]
                  (if (= x 0)
                    [input target b-offset input-data output-data :ok]
                    [input (+ sp 3) b-offset input-data output-data :ok])) 2 0] ;; jump if false
    (= code 7) [(fn [[input sp b-offset input-data output-data] x y target] ;;
                  (if (< x y)
                    [(assoc input target 1) (+ sp 4) b-offset input-data output-data :ok]
                    [(assoc input target 0) (+ sp 4) b-offset input-data output-data :ok])) 2 1] ;; less than
    (= code 8) [(fn [[input sp b-offset input-data output-data] x y target]
                  (if (= x y)
                    [(assoc input target 1) (+ sp 4) b-offset input-data output-data :ok]
                    [(assoc input target 0) (+ sp 4) b-offset input-data output-data :ok])) 2 1] ;; equals
    (= code 9) [(fn [[input sp b-offset input-data output-data] target]
                  ;;(println "modifiing bo: " b-offset " new " (+ b-offset target))
                  [input (+ 2 sp) (+ b-offset target) input-data output-data :ok]) 1 0] ;; modify base offset 1 read param 0 store param
    (= code 99) [(fn [[input sp b-offset input-data output-data]]
                   [input sp b-offset input-data output-data :done]) 0 0]
    :else [(fn [[input sp b-offset input-data output-data]]
             (println "Error halting sp: " sp)
             [input sp b-offset input-data output-data :done]) 0 0]))

(defn apply-opcode [world opcode first-param]
  (let [code (int (mod opcode 100))
        a (int (mod (/ opcode 100) 10))
        b (int (mod (/ opcode 1000) 10))
        c (int (mod (/ opcode 10000) 10))
        [input sp relative-base in-data out-data status]  world
        ;;bbb (println "opcode " opcode " first-param " first-param " relative-base: " relative-base)
        ;; fix for authors' incompetence; we need no-output-params separated
        ;; as it is always read by value mode == 1 and not indirectly by mode == 0
        ;; and it is modified by mode 2 as well!!!
        [action no-params no-output-params] (get-op code)
        values [(subvec input first-param (+ first-param no-params)) (subvec [a b c] 0 no-params)]
        out-values [(subvec input (+ first-param no-params) (+ first-param no-params no-output-params)) (subvec [a b c] no-params (+ no-params no-output-params))]
        params (apply (partial map
                (fn [param mode]
                  (read-value input mode param relative-base)))
                      values)
        out-params (apply (partial map
                     (fn [param mode]
                       (read-output-value input mode param relative-base)))
                          out-values)
        ;;ff (println "params: " params)
        ;;ff (println "out-params: "out-params)
        params (concat params out-params)
        ;;(subvec input (+ first-param no-params)
        ;;                             (+ first-param no-params no-output-params)))
        ]
      (apply (partial action world) params)
    ))

(defn chomp-input
  [input sp base-offset in-data]
  (loop [world [input sp base-offset in-data []] ;; input and stack pointer
         iter 0 ;; just safety measure to prevent cycling
         ]
    (let [[new-input new-sp new-base-offset new-in-data new-out-data status]
          (apply-opcode world (nth (first world) (second world)) (inc (second world)))
          ;;aa (println  "sp: " new-sp "base" new-base-offset " in-data: " new-in-data " out-data: " new-out-data" status: " status)
          ]
      (cond
        (> iter 500000) :failure
        (not=  status :ok) [new-input new-sp new-base-offset new-in-data new-out-data status]
        :else (recur [new-input new-sp new-base-offset new-in-data new-out-data] (inc iter))
        ))))

(nth (chomp-input input 0 0 [1]) 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UTIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn permutation
  [perm]
  (if (empty? perm)
    (list '())
    (for [item perm
          tail (permutation (remove #(= item %) perm))]
      (cons item tail))))

(permutation [1 2 3])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part 1
(def produces-itself-input (enlarge-buffer 1000 [109,1, ;; bp to 1
                                                 204,-1, ;; out of +1 -1
                                                 1001,100,1,100, ;; ram[100] = ram[100] + 1
                                                 1008,100,16,101, ;; ram[100] == 16 -> ram[101] = 0
                                                 1006,101,0, ;; if ram[101] == 0
                                                 99]))
(do
  (println "=================")
  (println "=================")
  (println "=================")
  (nth (chomp-input (vec produces-itself-input) 0 0 []) 4))

(def produces-itself-input-t (enlarge-buffer 10 [109,1,204,-1, 99]))
(chomp-input (vec produces-itself-input-t) 0 0 [])

(def produces-large-number-input [1102,34915192,34915192,7,4,7,99,0])
(chomp-input produces-large-number-input 0 0 [])

(def produces-large-in-middle-input [104,1125899906842624,99])
(chomp-input produces-large-in-middle-input 0 0 [])

(do
  (println "================== starting ===================")
  (chomp-input [9 4 203 1 99 0] 0 0 [1111]))

(do
  (println "================== starting ===================")
  (chomp-input [109 2 203 1 204 2 99 0] 0 0 [1111]))

;; this fails for me now
(do
  (println "================== starting ===================")
  (chomp-input (vec (enlarge-buffer 5000 [
                1102,34463338,34463338,63, ;; writes sum of large to 63
                1007,63,34463338,63, ;; less than, writes value of 0 on 63
                1005,63,53, ;; jump if true, we have false
                1101,0,3,1000, ;; write 4 sum to pos 1000
                109,988, ;; bp to 988, this is strange! have modifirer 1 for output param!
                209,12, ;; bp to 988 + 12 = 1000 or 1998
                9,1000, ;; bp 1000 + 1000 = 2000?
                209,6,
                209,3,
                203,0,
                1008,1000,1,63,
                1005,63,65,
                1008,1000,2,63,
                1005,63,904,
                1008,1000,0,63,
                1005,63,58,4,25,
                104,0,99])) 0 0 [1]))

(println "Part 1: "(nth (chomp-input input 0 0 [1]) 4))
(println "Part 2: "(nth (chomp-input input 0 0 [2]) 4))
