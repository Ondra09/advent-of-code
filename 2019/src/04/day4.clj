(ns advent.day4)
(use 'clojure.test)

;; let's try this solve it combinatorically
;; numbers that are ordered are combinations with repetition
;; because we are selecting exactly one combination of numbers
;; that is correct
;; in reality does not matter if we have 12 or 21
;; for combinations this is exactly the same number and one
;; of them is correct and holds that has ordered items

(defn fact
  ([n] (fact n 1))
  ([n f]
   (if (<= n 1)
     f
     (recur (dec n) (* f n)))))

(deftest fact-test
  (is (= 1 (fact 0)))
  (is (= 1 (fact 1)))
  (is (= 2 (fact 2)))
  (is (= 6 (fact 3)))
  (is (= 24 (fact 4)))
  (is (= 120 (fact 5))))

(defn n-over-k [n k]
  (if (or (> 0 k) (> k n))
    0
    (/ (fact n)
       (* (fact k) (fact (- n k))))))

(deftest n-over-k-test
  (is (= 1 (n-over-k 5 0)))
  (is (= 1 (n-over-k 0 0)))
  (is (= 5 (n-over-k 5 1)))
  (is (= 21 (n-over-k 7 2)))
  (is (= 1287 (n-over-k 13 5))))

;; todo add input checks
;; naive implementation
;; todo this overflows for n larger than 15
;; good enough for Day 4, however.
(defn combinations-rep [n k]
  "n is number of items, k is selecting number"
  (if (and (= n 0) (= k 0))
           1
           (n-over-k (- (+ n k) 1) k)))

(deftest combinats-rep-test
  (is (= 1 (combinations-rep 0 0)))
  (is (= 1 (combinations-rep 1 0)))
  (is (= 1 (combinations-rep 2 0)))
  (is (= 1 (combinations-rep 1 1)))
  (is (= 3 (combinations-rep 2 2)))
  (is (= 10 (combinations-rep 4 2)))
  (is (= 15 (combinations-rep 3 4)))
  (is (= 45 (combinations-rep 9 2)))
  (is (= 5005 (combinations-rep 10 6))))

(defn number-of-same-combinations [n k]
  "we have two options, one is that last number is the same as first generated
   second one is that it does not start with that number but we need to
   generate at least one double, so we select it"
  (let [starts-with-same-number (combinations-rep n (- k 1)) ;; we lost one position for the same number
        ;; we choose all positions without one neumber n - 1  and we need to substract all numbers that has no repeating
        ;; kombinations without repetition
        all-combinations-rest (if (and (> (- n 1) 0))
                                (combinations-rep (- n 1) (- k 0))
                                0)
        all-single-digits (if (>= (- n 1) k)
                            (n-over-k (- n 1) k)
                            0)
        starts-with-higher-number (if (> k 1) ;; k == 1 is a special case handled already above
                                    (- all-combinations-rest  all-single-digits)
                                    0)
        ]
    ;; (println "start leading: "  starts-with-same-number)
    ;; (println "all without leading: "  all-combinations-rest)
    ;; (println "all not repeating: "  all-single-digits  "n " n "k " k)

    (+ starts-with-same-number starts-with-higher-number)))

(deftest combinations-test
  ;; for numbers xx5yyy where three y are to be found and xx is already in order
  (is (= 31 (number-of-same-combinations (- 10 5) 3)))
  (is (= 19 (number-of-same-combinations (- 10 6) 3)))
  (is (= 7 (number-of-same-combinations (- 10 6) 2)))
  (is (= 3 (number-of-same-combinations (- 10 8) 2)))
  (is (= 1 (number-of-same-combinations (- 10 8) 1)))
  )

;; yay results
;; all numbers must be betwen 158126-624574
;; that means no number starts with 6
;; lest's find number of combinations for 6 digits
;; starting with 2, 3, 4, 5
;; and compute rest after 1588..
;; and 1589.. 16.... 17....


(time (println "result set 1: " (+ (number-of-same-combinations (- 10 2) 5)
   (number-of-same-combinations (- 10 3) 5)
   (number-of-same-combinations (- 10 4) 5)
   (number-of-same-combinations (- 10 5) 5)
   (number-of-same-combinations (- 10 8) 3)
   (number-of-same-combinations (- 10 9) 2)
   (number-of-same-combinations (- 10 6) 4)
   (number-of-same-combinations (- 10 7) 4)
   (number-of-same-combinations (- 10 8) 4)
   (number-of-same-combinations (- 10 9) 4))))

;; result set 1:  1665
;; "Elapsed time: 0.466586 msecs"
;; this is about 2000x faster than brute force

;; Different approach is here,
;; count of one double + two doubles and three doubles
;; combinations is computed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn number-of-doubles [n k]
  "Let's use the fact that we can have only 5 possible locatios to fill
  possible combinations: XXWYZ XXYYZ XXYYY"
  (let [one-double (if (> k 1) (* (n-over-k n 1) (n-over-k (- n 1) (- k 2)))
                       0);; one double and kombinations without repetitions
        two-double (if (> k 3) (* (n-over-k n 2) (n-over-k (- n 2) (- k 4)))
                       0)

        one-double-one-triple (if (> k 4) (* (n-over-k n 1) (n-over-k (- n 1) 1))
                                  0)

        summation (+ one-double two-double one-double-one-triple)
        ]
    ;; (println "one " one-double " two " two-double " two+thrd " one-double-one-triple)
    ;; (println "1x2: " (+ summation) " N: " n " k: " k)
    summation))

(deftest number-of-doubles-test
  (is (= 0 (number-of-doubles 5 1)))
  (is (= 1 (number-of-doubles 2 4)))
  (is (= 5 (number-of-doubles 5 2)))
  (is (= 2 (number-of-doubles 2 2)))
  (is (= 1 (number-of-doubles 1 2)))
  (is (= 6 (number-of-doubles 3 3)))
  (is (= 12 (number-of-doubles 4 3)))
  (is (= 20 (number-of-doubles 5 3)))
  )

;; newer version
(defn number-of-with-leading-double [n k]
  "for eg. 2xxxxx this will find all combinations that start with 22xxxxx"
  (let [valid-lead-double
        (combinations-rep (- n 1) (- k 1)) ;; 22 and rest can be anything without 2
        triplets (number-of-doubles (- n 1) (- k 2))
        quads (number-of-doubles (- n 1) (- k 3))
        sixths (number-of-doubles (- n 1) (- k 4))
        fifths (number-of-doubles (- n 1) (- k 5))
        result  (+ valid-lead-double triplets quads sixths fifths)
        ]
    ;; (println "leading: " valid-lead-double)
    ;; (println "result: " result)
    result
    ))

(deftest number-of-with-leading-double-test
  (is (= 1 (number-of-with-leading-double 2 1))) ;; 8X
  (is (= 1 (number-of-with-leading-double 1 1))) ;; 9X
  (is (= 1 (number-of-with-leading-double 2 2))) ;; 8XX
  (is (= 0 (number-of-with-leading-double 1 2))) ;; 9XX !!! this should be not accepted !!!
  (is (= 1 (number-of-with-leading-double 2 2))) ;; 8XX
  (is (= 2 (number-of-with-leading-double 3 2))) ;; 7XX only 77X is accepted
  (is (= 3 (number-of-with-leading-double 4 2))) ;; 6XX only 66X
  (is (= 6 (number-of-with-leading-double 4 3))) ;; 6XXX only 66XX
  (is (= 13 (number-of-with-leading-double 4 4))) ;; 6XXXX only 66XXX & 666XX!!!
  )

(defn number-of-same-combinations-2 [n k]
  (+ (number-of-with-leading-double n k)
     (number-of-doubles (- n 1) k)
     ))
(deftest part-test
  (is (= 546 (number-of-same-combinations-2 (- 10 2) 5))) ;; should be 546
  (is (= 312 (number-of-same-combinations-2 (- 10 3) 5))) ;;
  (is (= 165 (number-of-same-combinations-2 (- 10 4) 5))) ;;
  (is (= 79 (number-of-same-combinations-2 (- 10 5) 5))) ;;
  (is (= 1 (number-of-same-combinations-2 (- 10 8) 3)))
  (is (= 0 (number-of-same-combinations-2 (- 10 9) 2)))
  ;; 6xxxx
  ;; 66677 66688 66699 ;; 3
  ;; 66777 66778 66779 66788 66789 66799 66888 66889 66899 66999 ;; 10
  ;; 67788 67789 67799 67889 67899 68899 ;; 6

  (is (= 19 (number-of-same-combinations-2 (- 10 6) 4)))

  ;; 77888, 77889, 77899, 77999
  ;; 77788, 77799
  ;; 78899
  (is (= 7 (number-of-same-combinations-2 (- 10 7) 4)))

  (is (= 2 (number-of-same-combinations-2 (- 10 8) 4))) ;; 8XXXX, 88999, 88899
  (is (= 0 (number-of-same-combinations-2 (- 10 9) 4)))) ;;

(time (println "result set 1: "
               (+ (number-of-same-combinations-2 (- 10 2) 5)
                  (number-of-same-combinations-2 (- 10 3) 5)
                  (number-of-same-combinations-2 (- 10 4) 5)
                  (number-of-same-combinations-2 (- 10 5) 5)
                  (number-of-same-combinations-2 (- 10 8) 3)
                  (number-of-same-combinations-2 (- 10 9) 2)
                  (number-of-same-combinations-2 (- 10 6) 4)
                  (number-of-same-combinations-2 (- 10 7) 4)
                  (number-of-same-combinations-2 (- 10 8) 4)
                  (number-of-same-combinations-2 (- 10 9) 4))))

;; result set 1:  1131
;; "Elapsed time: 0.291504 msecs"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I admit, I cheat, here is brute force to get numbers to test against
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-growing? [number]
  (every?
   (fn [[x y]]
     (<= (int x) (int y)))
   (partition 2 1 (str number))))

(defn has-one-double? [number]
  (let [freq (frequencies (str number))]
    (some (fn [[x y]]
            (and (= x y)
                 (= (freq x) 2)))
          (partition 2 1 (str number)))))

(def test-2-pred?
  (every-pred
   is-growing?
   has-one-double?))

(defn part-2-count [input]
  (filter test-2-pred? input))

(time (count (part-2-count (range 158126 (inc 624574)))))
;; 1131
;; "Elapsed time: 685.98125 msecs"

(time (count (part-2-count (range 5000 (inc 6000)))))
(println (part-2-count (range 7000000 (inc 8000000))))
