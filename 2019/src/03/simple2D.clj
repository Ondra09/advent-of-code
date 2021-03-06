(ns advent.simple2D)
(use 'clojure.test)

(defn determinant [p0 p1]
  (let [a1 (:x p0)
        a2 (:y p0)
        b1 (:x p1)
        b2 (:y p1)]
    (- (* a1 b2) (* a2 b1))))


(determinant {:x 1 :y 5}
             {:x -5 :y 7})


(deftest determinant-test
  (is (= 5 5))
  (is (= (determinant {:x 1 :y 5}
                      {:x -5 :y 7}) 32))
  (is (= (determinant {:x -3 :y -1}
                      {:x -2 :y 3}) -11)))


(defn point-inside-line [xP p0 p1]
  (let [x1 (:x p0) x2 (:x p1)
        y1 (:y p0) y2 (:y p1)
        x  (:x xP) y  (:y xP)]
    (and
     (and (<= (min x1 x2) x) (<= x (max x1 x2)))
     (and (<= (min y1 y2) y) (<= y (max y1 y2))))))

(deftest inside-line-test
  (is (not (point-inside-line {:x -7 :y 3} {:x 5 :y 4} {:x -100 :y 4})))
  (is (not (point-inside-line {:x 0 :y 0} {:x 1 :y 0} {:x 1 :y 1})))
  )


(defn create-line-equation [A B]
  "equation form is ax + b = c"
  (let [a (- (:y B) (:y A))
        b (- (:x A) (:x B))
        c (+ (* a (:x A)) (* b (:y A)))]
    {:a a :b b :c c}))


(deftest line-equation-test
  (is (= (create-line-equation {:x 5 :y 1} {:x 0 :y 0})
         {:a -1 :b 5 :c 0}))
  (is (= (create-line-equation {:x 0 :y 10} {:x 0 :y 0})
         {:a -10 :b 0 :c 0})))


(defn line-line-intersection [a0 a1
                              b0 b1]
  "returns point struct {:x x-coord :y y-coord} or nil"
  (let [row-eq-a (create-line-equation a0 a1)
        row-eq-b (create-line-equation b0 b1)
        det (determinant {:x (:a row-eq-a) :y (:b row-eq-a)}
                         {:x (:a row-eq-b) :y (:b row-eq-b)})]
    (if (= det 0)
      nil
      (let [x-denom (determinant {:x (:c row-eq-a) :y (:c row-eq-b)}
                                 {:x (:b row-eq-a) :y (:b row-eq-b)})
            y-denom (determinant {:x (:a row-eq-a) :y (:a row-eq-b)}
                                 {:x (:c row-eq-a) :y (:c row-eq-b)})
            resultP {:x (/ x-denom det)
                    :y (/ y-denom det)}]

        (if (and (point-inside-line resultP a0 a1)
                 (point-inside-line resultP b0 b1))
          resultP
          nil)))))

(deftest line-intersections-test
  (is (= {:x 1 :y 1}
         (line-line-intersection {:x 0 :y 1} {:x 10 :y 1}
                                 {:x 1 :y 10} {:x 1 :y -10})))
  (is (= {:x -7 :y 4}
         (line-line-intersection {:x 5 :y 4} {:x -100 :y 4}
                                 {:x -7 :y 10} {:x -7 :y -10})))
  (is (nil? (line-line-intersection {:x 5 :y 4} {:x -100 :y 4}
                                    {:x -7 :y 1} {:x -7 :y -10}))))

(defn abs [n] (max n (- n)))

(defn manhattan-distance [p0 p1]
  (+ (abs (- (:x p0) (:x p1)))
     (abs (- (:y p0) (:y p1)))
     ))

(deftest manhattan-distance-test
  (is (= (manhattan-distance {:x -7 :y 4} {:x -7 :y 4}) 0))
  (is (= (manhattan-distance {:x 0 :y 0} {:x 1 :y 1}) 2))
  (is (= (manhattan-distance {:x 2 :y 9} {:x -7 :y 4}) 14))
  )
