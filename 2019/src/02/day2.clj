(ns advent.day1)
(use 'clojure.test)

;;;;;;;;;;;;;;;;;;;; This code is pbly ugly even to Cljoure standards
;;;;;;;;;;;;;;;;;;;; I am still learning

(defonce input [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,2,9,19,23,1,23,6,27,1,13,27,31,1,31,10,35,1,9,35,39,1,39,9,43,2,6,43,47,1,47,5,51,2,10,51,55,1,6,55,59,2,13,59,63,2,13,63,67,1,6,67,71,1,71,5,75,2,75,6,79,1,5,79,83,1,83,6,87,2,10,87,91,1,9,91,95,1,6,95,99,1,99,6,103,2,103,9,107,2,107,10,111,1,5,111,115,1,115,6,119,2,6,119,123,1,10,123,127,1,127,5,131,1,131,2,135,1,135,5,0,99,2,0,14,0])


(defn fix-1202-data [data]
  (let [d1 (assoc data 1 12)]
    (assoc d1 2 2)))

(defonce fixed-input (fix-1202-data input))

(defn get-op [code]
  (cond
    (= code 1) +
    (= code 2) *
    (= code 99) (do ;; (println "Exit")
                    :exit)
    :else (do (println "Error, haltin., value: " code)
              :error)))

(defn apply-opcode [data code loc1 loc2 target]
  (let [op (get-op code)
        val1 (nth data loc1)
        val2 (nth data loc2)]
    (if (fn? op)
      (assoc data target (op val1 val2))
      op)))

(deftest test-apply-opcode
  (is (= (nth (apply-opcode [0 1 2 3 4] 2 1 2 0) 0) 2))
  (is (= (nth (apply-opcode [0 1 2 3 4] 1 1 2 0) 0) 3))
  (is (= (nth (apply-opcode [0 1 2 3 4] 2 1 2 2) 2) 2)))

(defn chomp-input [data i]
  (let [result (apply apply-opcode data (subvec data i (+ i 4)))]
    (cond
      (vector? result) (chomp-input result (+ i 4))
      (= result :exit) (do
                         (nth data 0))
      :else result
          )))

(chomp-input fixed-input 0)

(apply apply-opcode fixed-input [1 0 2 4])

(fix-1202-data input)
(report-result-part-1 input)

;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; PART TWO
;;;;;;;;;;;;;;;;;;;;;;

(defonce expected-output 19690720)
(defn convert-to-result [noun verb]
  (+ (* 100 noun) verb))

(deftest example-answer
  (is (= 1202 (convert-to-result 12 2))))


(defn fix-input-data [data noun verb]
  (assoc (assoc data 1 noun) 2 verb))


(deftest fixing-input
  (is (= (fix-input-data [0 0 0 0] 10 20) [0 10 20 0])))


(doseq [x (range 100) y (range 100)]
  (let [test-input (fix-input-data input x y)
        result (chomp-input test-input 0)]
    (if (= expected-output result)
      (do
        (println x " " y " => " result)
        (println "converted result is: " (convert-to-result x y))))))
