(ns day_07.day7
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            ))

(defn split-input [s]
  (str/split s #","))

(def input (->> (slurp "input.txt")
                (str/trim)
                (split-input)
                (map #(Integer/parseInt %))
                (vec)))
(count input)
(defn read-value [data mode val]
  (if (= mode 1)
    val
    (nth data val)
    ))

(defn write-address [data address-to-write value]
  (assoc data value))


(defn get-op [code]
  "returns vector [fun advance-no]"
  (cond
    (= code 1) [(fn [[input sp input-data output-data] x y target]
                  [(assoc input target (+ x y)) (+ sp 4) input-data output-data :ok]) 2 1] ;; add 2 params, store function
    (= code 2) [(fn [[input sp input-data output-data] x y target]
                  ;;(println "Code2 : " target "  " (nth input target))
                  [(assoc input target (* x y)) (+ sp 4) input-data output-data :ok]) 2 1] ;; mul
    (= code 3) [(fn [[input sp input-data output-data] target]
                  (if (empty? input-data)
                    [input sp input-data output-data :block]
                    [(assoc input target (first input-data)) (+ sp 2) (drop 1 input-data) output-data :ok])) 0 1] ;; store
    (= code 4) [(fn [[input sp input-data output-data] x]
                  ;; (println x)
                  [input (+ sp 2) input-data  (conj output-data x) :ok]) 1 0] ;; output
    (= code 5) [(fn [[input sp input-data output-data] x target]
                  (if (> x 0)
                    [input target input-data output-data :ok]
                    [input (+ sp 3) input-data output-data :ok])) 2 0] ;; jump if true
    (= code 6) [(fn [[input sp input-data output-data] x target]
                  (if (= x 0)
                    [input target input-data output-data :ok]
                    [input (+ sp 3) input-data output-data :ok])) 2 0] ;; jump if false
    (= code 7) [(fn [[input sp input-data output-data] x y target]
                  (if (< x y)
                    [(assoc input target 1) (+ sp 4) input-data output-data :ok]
                    [(assoc input target 0) (+ sp 4) input-data output-data :ok])) 2 1] ;; less than
    (= code 8) [(fn [[input sp input-data output-data] x y target]
                  (if (= x y)
                    [(assoc input target 1) (+ sp 4) input-data output-data :ok]
                    [(assoc input target 0) (+ sp 4) input-data output-data :ok])) 2 1] ;; equals
    (= code 99) [(fn [[input sp input-data output-data]]
                   [input sp input-data output-data :done]) 0 0]
    :else [(fn [[input sp input-data output-data]]
             (println "Error halting sp: " sp)
             [input sp input-data output-data :done]) 0 0]))

(defn apply-opcode [world opcode first-param]
  (let [;;bbb (println "opcode " opcode " first-param " first-param)
        code (int (mod opcode 100))
        a (int (mod (/ opcode 100) 10))
        b (int (mod (/ opcode 1000) 10))
        c (int (mod (/ opcode 10000) 10))
        data (first world)
        ;; fix for authors' incompetence; we need output-param separated
        ;; as it is always read by value mode == 1 and not indirectly by mode == 0
        [action no-params output-param] (get-op code)
        values [(subvec data first-param (+ first-param no-params)) (subvec [a b c] 0 no-params)]
        params (apply (partial map
                (fn [param mode]
                  (read-value data mode param)))
                      values)
        params (concat params (subvec data (+ first-param no-params)
                                      (+ first-param no-params output-param)))
        ]
      (apply (partial action world) params)
    ))

(defn chomp-input
  [input sp in-data]
  (loop [world [input sp in-data []] ;; input and stack pointer
         iter 0 ;; just safety measure to prevent cycling
         ]
    (let [[new-input new-sp new-in-data new-out-data status]
          (apply-opcode world (nth (first world) (second world)) (inc (second world)))
          ;;aa (println "dd: " sp " in-data: " in-data " status: " status)
          ]
      (cond
        (> iter 500) :failure
        (not=  status :ok) [new-input new-sp new-in-data new-out-data status]
        :else (recur [new-input new-sp new-in-data new-out-data] (inc iter))
        ))))

(chomp-input input 0 [1 ])

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
(do  ;; this is test, resutl should be 81
  (println "\n============ starting ============")
  (let [[input sp in-data out-data status] (chomp-input input 0 [1 0])]
    [status out-data]))

;; part 1

(defn initialize-machines-with-phase [phase]
  (reduce
   (fn [[prev & rest] current]
     (let [[input sp in-data [out-data & out-data-tail]] (chomp-input input 0 [current prev])]
       ;; (println (first rest))
       [out-data (conj (first rest) sp)]))
   [0 []] phase))

(initialize-machines-with-phase [1 2 3 0 4])
(initialize-machines-with-phase [6 5 0])

;; 199988
(->> (permutation (range 5))
     (map
      #(initialize-machines-with-phase %1))
     (map #(first %1))
     (apply max))

;; part 2

(->> (permutation (range 5 10))
     (map
      (fn [permutation]
        1)
      ))

(defn init-machines [phases]
  (for [phase phases]
    [input 0 [phase] :ok]))

(defn simulate-machines
  "Takes input and pass it to first machine, result to second and so on."
  [data-input machines]
  (loop [machines-in machines
         data-input data-input
         machines-out []]
    (if (empty? machines-in)
      (do
        ;; (println (map last machines-out))
         [machines-out data-input])
      (let [[machine-input machine-sp machine-input-data status] (first machines-in)
            [new-input new-sp new-in-data new-out-data status]
            (chomp-input machine-input machine-sp (concat machine-input-data data-input))]

        (recur (drop 1 machines-in) new-out-data (conj machines-out [new-input new-sp [] status]))
        )
      )))


(defn simulate-until-result [machines]
  (loop [machines machines
         input [0]]
  (let [all-done (every? (fn [machine]
                           (let [status (nth machine 3)
                                 done (or (= status :failure) (= status :done))]
                             done))
                             machines)]
    (if all-done
      input
      (let [[machines out] (simulate-machines input machines)]
        (recur machines out))
    ))))

(simulate-until-result (init-machines [5 7 6 9 8]))

(println (->> (permutation (range 5 10))
              (map
               #(->> (init-machines %1)
                     (simulate-until-result)
                     )
               )
              (map first)
              (apply max)))
