(ns util.simple2D)
(use 'clojure.test)

(defn determinant
  ([a1 a2 b1 b2] (- (* a1 b2) (* a2 b1)))
  ([p0 p1]
   (let [a1 (:x p0)
         a2 (:y p0)
         b1 (:x p1)
         b2 (:y p1)]
     (determinant a1 a2 b1 b2))))


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
  "equation form is ax + by = c"
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

(defn vec2D
  [ax ay
   bx by]
  [(- bx ax) (- by ay)])

(defn vec-length [x y]
  (Math/sqrt (+ (* x x) (* y y))))

(defn vec-normalize [x y]
  (let [length (vec-length x y)]
    (if (= length 0.0)
      [x y]
      [(/ x length) (/ y length)])))

(defn vec-dot-p [ax ay bx by]
  (+ (* ax  bx) (* ay by)))

(defn vec-cross [ax ay bx by]
  "value is sin of angle between vectors"
  (determinant ax ay bx by))

(defn vec-diff->angle [^double basex ^double basey ^double ax ^double ay]
  "Vectors should be normalized."
  (let [cos (vec-dot-p basex basey ax ay)
        sin (vec-cross basex basey ax ay)]
    (Math/atan2 sin cos)))

(defn normalize-angle [^double angle]
  "normalizes angle to 0..2PI"
  (mod angle (* 2 Math/PI)))

(deftest vec-diff->angle-test
  (is (= 0.0 (vec-diff->angle 1 0 1 0)))
  (is (= (/ Math/PI 2) (vec-diff->angle 1 0 0 1)))
  (is (= Math/PI (vec-diff->angle 1 0 -1 0)))
  (is (= (/ Math/PI -2) (vec-diff->angle 1 0 0 -1))))

;; taken from https://rosettacode.org/wiki/Least_common_multiple#Clojure
(defn gcd
      [a b]
      (if (zero? b)
      a
      (recur b, (mod a b))))

(defn lcm
      [a b]
      (/ (* a b) (gcd a b)))
;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))


;;;;;;;;;;;;;;;;;;
(def infinity Long/MAX_VALUE)

(defn dijkstra [dst-table starting-node]
  (loop [to-visit #{starting-node}
         computed-dists (transient {starting-node 0})
         visited #{}]
    (if  (not (seq to-visit))
      (persistent! computed-dists)
      (let [current-node (first to-visit)
            new-visited (conj visited current-node)
            dsts (get dst-table current-node)
            curr-dst-to-node (get computed-dists current-node infinity)
            [new-computed-dists new-to-visit] (reduce (fn [[coll to-visit-coll] [k v]]
                                                        (let [curr-val (get coll k infinity)]
                                                          (if (or (> curr-val (+ curr-dst-to-node v)))
                                                            (do
                                                              [(assoc! coll k (+ curr-dst-to-node v))
                                                               (conj to-visit-coll k)])
                                                            [coll to-visit-coll]))) [computed-dists (rest to-visit)] dsts)]


        (recur new-to-visit new-computed-dists new-visited)))))

(deftest dijkstra-test
  (let [dst-table {:a {:b 2 :c 150}
                   :b {:a 2 :c 3}
                   :c {:b 3}}
        dst-table-2 {:a {:b 2 :c 1}
                     :b {:a 2 :c 7 :d 78}
                     :c {:a 1 :b 7 :d 2}
                     :d {:b 6 :c 2}}
        dst-table-3 {:a {:b 5 :bb 1}
                     :b {:c 2}
                     :bb {:c 1}
                     :c {:d 2}
                     :d {:e 1}}
        dst-table-4 {:SC {:SC 0, :KQ 53, :IJ 61},
                     :ZZ {:ZZ 0, :MV 5, :AC 47},
                     :JL {:JL 0, :FW 5, :YM 49, :UJ 53, :JQ 51},
                     :MF {:MF 0, :FW 61, :XQ 65},
                     :SJ {:SJ 0, :SD 73, :UI 51},
                     :UJ {:UJ 0, :YM 7, :JL 53, :FW 55, :UI 49},
                     :AC {:AC 0, :YJ 55, :MV 45, :ZZ 47},
                     :JA {:JA 0, :SD 53, :OY 37},
                     :OY {:OY 0, :YT 47, :JA 37},
                     :YM {:YM 0, :UJ 7, :JL 49, :FW 51, :LE 43},
                     :YL {:YL 0, :LE 81, :AA 53, :UX 57},
                     :LE {:LE 0, :YM 43, :YL 81},
                     :RA {:RA 0, :YJ 39, :NC 55},
                     :SD {:SD 0, :SJ 73, :JA 53},
                     :YJ {:YJ 0, :RA 39, :AC 55},
                     :ZB {:ZB 0, :UX 49, :XQ 51},
                     :IJ {:IJ 0, :PS 45, :SC 61},
                     :PS {:PS 0, :IJ 45, :MV 37},
                     :FW {:FW 0, :MF 61, :JL 5, :YM 51, :UJ 55},
                     :AA {:AA 0, :UX 7, :YL 53},
                     :JQ {:JQ 0, :JL 51, :KQ 49},
                     :XQ {:XQ 0, :MF 65, :ZB 51},
                     :ND {:ND 0, :NC 59, :YT 61},
                     :MV {:MV 0, :PS 37, :ZZ 5, :AC 45},
                     :UX {:UX 0, :ZB 49, :AA 7, :YL 57},
                     :NC {:NC 0, :ND 59, :RA 55},
                     :YT {:YT 0, :ND 61, :OY 47},
                     :KQ {:KQ 0, :SC 53, :JQ 49},
                     :UI {:UI 0, :SJ 51, :UJ 49}}]

    (is (= (dijkstra dst-table :a) {:a 0 :b 2 :c 5}))
    (is (= (dijkstra dst-table :b) {:a 2 :b 0 :c 3}))
    (is (= (dijkstra dst-table :c) {:a 5 :b 3 :c 0}))

    (is (= (dijkstra dst-table-2 :a) {:a 0 :b 2 :c 1 :d 3}))
    (is (= (dijkstra dst-table-2 :c) {:a 1 :b 3 :c 0 :d 2}))

    (is (= (dijkstra dst-table-3 :a) {:a 0 :b 5 :bb 1 :c 2 :d 4}))
    (is (= (dijkstra dst-table-3 :bb) {:bb 0 :c 1 :d 3}))
    (is (= (get (dijkstra dst-table-4 :AA) :ZZ) 527))))


;;;;;;;;;;;;;;;;;; 2d array manip ;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype Board [^int line-length ^int lines  data])

(defn coords->idx [^Board board x y]
  (+ x (* (.line-length board) y)))

(defn idx->coords [^Board board idx]
  [(int (mod idx (.line-length board))) (int (/ idx (.line-length board)))])

(defn in-board? [^Board board x y]
  (and (>= x 0) (< x (.line-length board))
       (>= y 0) (< y (.lines board))))

(defn get-xy-unchecked [^Board board x y]
  (nth (.data board) (coords->idx board x y)))


(defn get-xy [^Board board x y]
  (when (in-board? board x y)
    (get-xy-unchecked board x y)))

;; gets neighbours, TODO: abstract function that gets neigbours to be able to change
;; how it is obtained. This gets cross neighbours
;; (defn get-neighbourhood [^Board board x y]
;;   [(inc x) y
;;    (dec x) y
;;    x (inc y)
;;    x (dec y)])
