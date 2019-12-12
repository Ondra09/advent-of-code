(ns advent.day5)
(use 'clojure.test)

(defonce input [3,225,1,225,6,6,1100,1,238,225,104,0,1101,40,27,224,101,-67,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,33,38,225,1102,84,60,225,1101,65,62,225,1002,36,13,224,1001,224,-494,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,1102,86,5,224,101,-430,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1102,23,50,225,1001,44,10,224,101,-72,224,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,102,47,217,224,1001,224,-2303,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,71,84,225,101,91,40,224,1001,224,-151,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,87,91,225,1102,71,19,225,1,92,140,224,101,-134,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,2,170,165,224,1001,224,-1653,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1101,49,32,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,226,677,224,1002,223,2,223,1006,224,329,101,1,223,223,8,226,226,224,1002,223,2,223,1005,224,344,101,1,223,223,1007,677,226,224,102,2,223,223,1005,224,359,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,404,1001,223,1,223,108,677,677,224,1002,223,2,223,1006,224,419,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,434,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,464,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,1108,677,677,224,1002,223,2,223,1005,224,494,101,1,223,223,7,677,677,224,1002,223,2,223,1005,224,509,101,1,223,223,1007,677,677,224,1002,223,2,223,1005,224,524,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,554,101,1,223,223,107,226,677,224,1002,223,2,223,1005,224,569,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,584,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,599,1001,223,1,223,1008,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,629,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226])


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
    (= code 1) [(fn [[data sp] x y target]
                  [(assoc data target (+ x y)) (+ sp 4)]) 2 1] ;; add 2 params, store function
    (= code 2) [(fn [[data sp] x y target]
                  [(assoc data target (* x y)) (+ sp 4)]) 2 1] ;; mul
    (= code 3) [(fn [[data sp] target]
                  [(assoc data target (read-string (read-line))) (+ sp 2)]) 0 1] ;; store
    (= code 4) [(fn [[data sp] x]
                  (println x)
                  [data (+ sp 2)]) 1 0] ;; output

    (= code 5) [(fn [[data sp] x target]
                  (if (> x 0)
                    [data target]
                    [data (+ sp 3)])) 2 0] ;; jump if true
    (= code 6) [(fn [[data sp] x target]
                  (if (= x 0)
                    [data target]
                    [data (+ sp 3)])) 2 0] ;; jump if false
    (= code 7) [(fn [[data sp] x y target]
                  (if (< x y)
                    [(assoc data target 1) (+ sp 4)]
                    [(assoc data target 0) (+ sp 4)])) 2 1] ;; less than
    (= code 8) [(fn [[data sp] x y target]
                  (if (= x y)
                    [(assoc data target 1) (+ sp 4)]
                    [(assoc data target 0) (+ sp 4)])) 2 1] ;; equals
    (= code 99) (do  ;; (println "Exit")
                  [:exit 0 0])
    :else (do (println "Error, halting. Value: " code)
              [:error 0 0])))

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
        ;; ccc (println "no params " no-params)
        values [(subvec data first-param (+ first-param no-params)) (subvec [a b c] 0 no-params)]
        params (apply (partial map
                (fn [param mode]
                  ;; (println "param " param " mode " mode)
                  (read-value data mode param)))
                      values)
        params (concat params (subvec data (+ first-param no-params)
                                      (+ first-param no-params output-param)))
        ]
     ;;(println" values "  values " params " params "\n")

    (comment
      (if (fn? action)
        (let [val-to-store (apply action params)]
          [(if (fn? store-fun)
             (store-fun data (nth data (+ first-param no-params)) val-to-store)
             data)
           shift-ip ;; if have store par, use inc otherwise we should not increase
           :ok])
        [data shift-ip action]))
    (if (fn? action)
      [(apply (partial action world) params) :ok]
      [world action])
    ))

(defn chomp-input [input]
  (loop [world [input 0]
         iter 0]

    (let [[new-data end] (apply-opcode world (apply nth world) (inc (second world)))]
      (cond
        (> iter 500) :failure
        (not=  end :ok) end
        :else (recur new-data (inc iter))
        ))))

(do
  (println "\n============ starting ============")
  (chomp-input input))


(defonce test [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
               999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])

(chomp-input test)

(chomp-input [3,12,6,12,15,1,13,14,13,4,
              13,99,-1,0,1,9]) ;; 10 .. 15
