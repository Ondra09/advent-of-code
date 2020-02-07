(ns day_20.day20
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(use 'clojure.test)

(load-file "../util/simple2D.clj")
(clojure.core/alias 's2D 'util.simple2D)

(defn max-line-length [input]
  (->> input
       str/split-lines
       (map count)
       (apply max)))

;; not optimal to split input twice, but is used only
;; once and for short input
(defn parse-input [input]
  (let [line-length (max-line-length input)
        lines (str/split-lines input)
        lines-no (count lines)]
    [line-length lines-no (->> lines
                               ;; append spaces to have all the same length
                               (map #(apply (partial str %) (repeat (- line-length (count %)) \space)))
                               (apply str))]))

(def input (->> (slurp "input.txt")))
;;(def input (->> (slurp "input2.txt")))
(def test-input
  "         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z       ");

(def tt (apply s2D/->Board (parse-input input)))

(defn is-doors [char0 char1 char2]
  (when (and (Character/isUpperCase char0) (Character/isUpperCase char1) (= \. char2)) (str char0 char1)))

(deftest is-doors-test
  (is (= (is-doors \Z \E \.) "ZE"))
  (is (= (is-doors \z \E \.) nil))
  (is (= (is-doors \Z \E \E) nil)))

(defn check-doors [board [x0 y0] [x1 y1] [x2 y2]]
  (when (and (s2D/in-board? board x0 y0)
             (s2D/in-board? board x1 y1)
             (s2D/in-board? board x2 y2))
    (is-doors (s2D/get-xy board x0 y0)
              (s2D/get-xy board x1 y1)
              (s2D/get-xy board x2 y2))))

(defn create-doors [name coords]
  (when name [name coords]))

(defn passable? [character]
  (= character \.))

(defn is-doors-char? [character]
  (Character/isUpperCase character))

(defn parse-doors [board [x y]]
  (let [char (s2D/get-xy board x y)
        are-doors-down  (create-doors (check-doors board [x y] [x (inc y)] [x (+ y 2)]) [x (inc y)])
        are-doors-up    (create-doors (check-doors board [x (dec y)] [x y] [x (- y 2)]) [x (dec y)]) ;; to have doors name in the same order
        are-doors-right (create-doors (check-doors board [x y] [(inc x) y] [(+ x 2) y]) [(inc x) y])
        are-doors-left  (create-doors (check-doors board [(dec x) y] [x y] [(- x 2) y]) [(dec x) y])]
    (filter (comp not nil?) [are-doors-down are-doors-up are-doors-right are-doors-left])))

(defn find-all-doors [board]
  (reduce (fn [coll idx] (let [doors (parse-doors board (s2D/idx->coords board idx))] (into coll doors))) [] (seq (range (* (.line-length board)
                                                                                                                            (.lines board))))))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn coords->doors [doors-coll x y]
  "return doors if located at x y, otherwise nil"
  (loop [coll doors-coll]
    (if (not (seq coll))
      nil
      (let [[door [xx yy]] (first coll)]
        (if (and (= x xx) (= y yy))
          door
          (recur (rest coll)))))))

;; (coords->doors '(["AA" [0 0]] ["BB" [3 5]]) 4 5)

(defn in-board? [board [x y]]
  (s2D/in-board? board x y))

(defn to-passable [board visited all-doors]
  "creates transducer that filter list of input board coords and filter passable only"
  (let [in-board? (partial in-board? board)
        not-already-seen? (fn [[x y]] (not (contains? visited [x y])))
        passable? (fn [[x y]] (or (passable? (s2D/get-xy board x y))
                                  (and
                                   (is-doors-char? (s2D/get-xy board x y))
                                   (not (nil? (coords->doors all-doors  x y))))))]

    (comp
     (filter not-already-seen?)
     (filter in-board?)
     (filter passable?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-neighbourhood [x y]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])


(defn find-neighbours [board all-doors [door-name [x y]]]
  (loop [found #{[x y]}
         q (queue [[x y -1]]) ;; ok, we start next to doors as 0
         result {}
         n 0]
    (if (or (empty? q) (> n 500000))
      result
      (let [[x y dst] (peek q)
            new-result(if (and (is-doors-char? (s2D/get-xy board x y)) (coords->doors all-doors  x y))
                        (assoc result (coords->doors all-doors  x y) (max 0 dst)) ;; max is to prevent have -1 for self
                        result)
            passable (transduce (to-passable tt found all-doors) conj '() (get-neighbourhood x y))
            new-found (reduce conj found passable)
            new-queue (reduce (fn [found [x y]](conj found [x y (inc dst)])) (pop q) passable)]
        (recur new-found new-queue new-result (inc n))))))


;; (println "")
;; (find-neighbours tt (find-all-doors tt) ["AA" [9 1]])
;; (find-all-doors tt)

(defn find-all-paths [board]
  (let [all-doors  (find-all-doors tt)]
    (reduce (fn [coll [name [x y]]]
              ;; (update-in {"AA" {"ff" 554}} ["AA"] #(assoc %1 "AA" 55))
              (update-in coll [name] #(apply (partial assoc %1) (flatten (vec (find-neighbours board all-doors [name [x y]]))))))
            ;;(assoc coll name ))

            {} all-doors)))

;; 526
(println "Part 1: "
         (dec (get (s2D/dijkstra (find-all-paths tt) "AA") "ZZ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-outer [board x y]
  (or (= x 1) (= y 1)
      (= x (- (.line-length board) 2)) (= y (- (.lines board) 2))))

(defn find-neighbours-2 [board all-doors [[door-name level] [x y]]]
  (loop [found #{[x y]}
         q (queue [[x y -1]]) ;; ok, we start next to doors as 0
         result {}
         n 0]
    (if (or (empty? q) (> n 500000))
      result
      (let [[x y dst] (peek q)
            new-result(if (and (coords->doors all-doors  x y) (not (= door-name (coords->doors all-doors  x y))))
                        (assoc result [(coords->doors all-doors x y) (if (is-outer board x y) level (inc level))] (max 0 dst)) ;; max is to prevent have -1 for self
                        result)
            passable (transduce (to-passable tt found all-doors) conj '() (get-neighbourhood x y))
            new-found (reduce conj found passable)
            new-queue (reduce (fn [found [x y]](conj found [x y (inc dst)])) (pop q) passable)]
        (recur new-found new-queue new-result (inc n))))))

;; not an tight loop no transducers needed
(defn filter-by [f coll]
  (->> coll
       (filter (fn [[[name level] n]] (f name level n)))
       (into {})))

(defn filter-portals [portals]
  (filter-by (fn [name level rest] (or
                                    (and
                                     (= 0 level)
                                     (or
                                      (= "AA" name)
                                      (= "ZZ" name)))
                                    (and
                                     (> level 0)
                                     (not (= "AA" name))
                                     (not (= "ZZ" name))))) portals))

(deftest filter-correctly
  (is (= (filter-portals {["YM" 0] {["UJ" 0] 7, ["JL" 1] 49, ["FW" 1] 51}}) {}))
  (is (= (filter-portals {["AF" 0] {["AA" 1] 7, ["JL" 1] 49, ["FW" 1] 51}
                          ["AA" 1] {["AA" 1] 7, ["JL" 1] 49, ["FW" 1] 51}
                          ["ZZ" 1] {["AA" 1] 7, ["JL" 1] 49, ["FW" 2] 51}}) {}))
  (is (= (filter-portals {["AC" 2] {["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51}})
         {["AC" 2] {["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51}}))
  (is (= (count (filter-portals {["AC" 2] {["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51}
                                 ["AA" 2] {["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51}
                                 ["AC" 0] {["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51}
                                 ["ZZ" 2] {["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51}
                                 ["UX" 0] {["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51}
                                 ["AA" 0] {["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51}
                                 ["ZZ" 0] {["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51}})) 3)))

(defn filter-beg-end [coll]
  "removes ALL ZZs and AAs "
  (filter (fn [[[name lvl] val]] (println lvl) (not (or (= name "AA")
                                                        (= name "ZZ")))) coll))

;; (filter-beg-end {["ZZ" 0] 5, ["AA" 2] 7, ["JL" 0] 49, ["FW" 4] 51})

(defn find-all-paths-2 [board levels]
  (let [all-doors  (find-all-doors board)]
    (loop [level 0
           doors {}]
      (if (= level levels)
        doors
        (recur (inc level)
               (merge-with into doors (filter-portals (reduce (fn [coll [name [x y]]]
                                                                (assoc coll [name (if (is-outer board x y) level (inc level))]  (find-neighbours-2 board all-doors [[name level] [x y]])))
                                                              {} all-doors))))))))

(def levels-30 (find-all-paths-2 tt 50))

;; 6293
(println "Part 2:"
         (dec (get (s2D/dijkstra levels-30 ["AA" 0]) ["ZZ" 0])))
