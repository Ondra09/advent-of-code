(ns day_18.day18
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(use 'clojure.test)

(load-file "../util/simple2D.clj")
(clojure.core/alias 's2D 'util.simple2D)

(def test-input
  "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################") ;; 132 steps

(def test-input-2
  "#########
#b.A.@.a#
#########") ;; 8 steps

(def test-input-3
  "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################");; 136

(def test-input-4
  "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################") ;; 81

(def input (->> (slurp "input.txt")
                (str/trim)))

;;(def input test-input-)

;; (println input)

(deftype visited-node [^int index ^java.util.Set keys-taken ^int steps]
  Object
  (equals [this other]
    (and (= (.index this) (.index other))
         (= (.keys-taken this) (.keys-taken other))))
  (toString [this]
    (str "<" (.index this) ", " (.keys-taken this) ", " (.steps this) ">"))
  (hashCode [this]
    (hash {:index (.index this) :keys-taken (.keys-taken this)}))
  Comparable
  (compareTo [this that]
    (compare [(.index this) (.index this)]
             [(.keys-taken that) (.keys-taken that)])))

;; (visited-node. 4 #{})
;; (.index )

(defn get-xy-board [width]
  (fn [array x y]
    (nth array (+ x (* y width)))))

(defn idx->coords-board [width]
  (fn [idx]
    [(mod idx width) (int (/ idx width))]))

(defn coords->idx-board [width]
  (fn [[x y]] (+ x (* y width))))

(defn row-length [input]
  (.indexOf input (int \newline)))

(defn board-map [input] (flatten (partition (row-length input) (inc (row-length input)) input)))

;; (row-length input)

(def main-board (vec (board-map input)))

(def get-xy (get-xy-board (row-length input)))
(def idx->coords (idx->coords-board (row-length input)))
(def coords->idx (coords->idx-board (row-length input)))

(comment
  (defn get-neighbourhood-board [coll]
    (fn [[x y]]
      [(get-xy coll (inc x) y)
       (get-xy coll (dec x) y)
       (get-xy coll x (inc y))
       (get-xy coll x (dec y))]))

  (defn get-neighbourhood [coll [x y]]
    [(get-xy coll (inc x) y)
     (get-xy coll (dec x) y)
     (get-xy coll x (inc y))
     (get-xy coll x (dec y))])

  (get-neighbourhood main-board [40 40])
  )

(defn get-neighbourhood-coords [[x y]]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn find-char [coll character]
  (let [j-idx (.indexOf coll character)]
    (if (not= -1 j-idx)
      j-idx
      nil)))

;; unused refactor and move to libs
(comment
  (defn create-distance-map [coll coords]
    (assoc (second (reduce (fn [[idx coll] coord] [(inc idx) (assoc coll (idx->coords idx) 999999)]) [0 {}] coll))
           coords 0))

  (defn distance-map [board passable? a] ;; TODO generalize to be used elsewhere use walls and board-passable, pass function and not list instead of walls
    (loop [distances (create-distance-map board a)
           to-search `(~a)]
      (if (empty? to-search)
        distances
        (let [node (first to-search)
              node-val (apply get-xy board node)
              node-dst (get distances node)
              neighbourhood-coords (if (passable? node-val) (get-neighbourhood-coords node) [])
              [to-search distances] (reduce (fn [[to-search dsts] coord] (if (> (get dsts coord) (inc node-dst))
                                                                           [(conj to-search coord) (assoc dsts coord (inc node-dst))]
                                                                           [to-search dsts])) [(rest to-search) distances] neighbourhood-coords)
              ]
          (recur distances to-search)
          ))))

  )

(defn board-key? [character]
  (Character/isLowerCase character))

(defn board-gate? [character]
  (Character/isUpperCase character))

(defn board-passable? [keys-taken character]
  (or (= \. character) (= \@ character) (board-key? character)
      (contains? keys-taken (Character/toLowerCase character))
      ))

(deftest board-passable-test
  (is (= true (board-passable? #{} \.)))
  (is (= true (board-passable? #{} \@)))
  (is (= true (board-passable? #{} \a)))
  (is (= false (board-passable? #{} \#)))
  (is (= true (board-passable? #{\a} \A)))
  (is (= true (board-passable? #{\f \z \a} \F)))
  (is (= true (board-passable? #{\f \z \a} \F)))
  (is (= false (board-passable? #{\z \a} \F)))
  (is (= true (board-passable? #{\z \a} \a)))
  (is (= false (board-passable? #{} \A))))

(defn not-walls? [character]
  (not= \# character))

(defn assoc-if-idx-not-nil [coll idx val]
  (if-not (nil? idx)
    (assoc coll idx val)
    coll))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
   (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;;
(defn add-key [board keys-coll idx]
  "adds key if it is on idx"
  (let [character (nth board idx)]
    (if (board-key? character)
      (conj keys-coll character)
      keys-coll)))


(defn count-all-keys [input]
  (reduce (fn [sum character] (if (board-key? character)
                                (inc sum)
                                sum)) 0 input))

;; (add-key main-board #{\c} 18)
;; .index
;; .keys-taken
(defn solve-maze [input keys-count]
  (loop [queue (queue `(~(visited-node. (find-char input \@) #{} 0)))
         visited #{}
         n 0]
    (if (or (= keys-count (count (.keys_taken (peek queue)))) (empty? queue))
      (peek queue)
      (let [node (peek queue)
            neighbours (map coords->idx (get-neighbourhood-coords (idx->coords (.index node))))
            keys-taken (.keys-taken node)
            passable (filter (comp (partial board-passable? keys-taken) (partial nth input)) neighbours)
            new-visited (conj visited node)
            new-nodes (map (fn [idx] (visited-node. idx (add-key input keys-taken idx) (inc (.steps node)))) passable)
            new-nodes-filtered (filter #(not (contains? visited %1)) new-nodes)
            new-queue (reduce #(conj %1 %2) queue new-nodes-filtered)]

        (recur (pop new-queue) new-visited (inc n))))))

;; this runs several hours, possible optimizitaion
;; 1. use bitset instead of set for storing foudn keys
;; 2. remove multpile map/filter/reduce callsi in every step and replace it with
;; single one
;; 3. use mutable data structures
(do
  (println "Part 1: steps "
           (.steps (solve-maze main-board (count-all-keys main-board)))))

;; ==============================================================================
;; ==================================== PART 2 ==================================
;; ==============================================================================


(defn modify-input [input idx]
  "Change input maze accepted as input for part 2, returns [new-board start-postions] "
  (let [[x y] (idx->coords idx)
        to-change [[x y] [(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]]
        new-start [[(inc x) (inc y)] [(dec x) (inc y)] [(dec x) (dec y)] [(inc x) (dec y)]]
        new-input (reduce (fn [coll coords] (assoc coll (coords->idx coords) \#)) input to-change)
        new-input (reduce (fn [coll coords] (assoc coll (coords->idx coords) \@)) new-input new-start)]

    [new-input (map #(coords->idx %1) new-start)]))

;; naive solution, just remove all doors that are not in quadrant, this won't work for e.g. this:
;; ##############
;; #@.........af#
;; #B############
;; #c#
;; ###

;; #####
;; #@Ab#
;; #####
;; vs.
;; #####
;; #@.b#
;; #####
;;
;; but it works for this input

(do (println "START")
    (map println
         (partition 81 (first (modify-input main-board (find-char main-board \@))))))

(defn find-all-keys-doors [input start-pos]
  (loop [keys '()
         doors '()
         q (queue `(~start-pos))
         visited (conj #{} start-pos)
         n 0]
    (if (or (empty? q) (> n 1030))
      [keys doors visited]
      (let [node (peek q)
            character (nth input node)
            new-keys (if (board-key? character) (conj keys character) keys)
            new-doors (if (board-gate? character) (conj doors character) doors)
            neighbours (map coords->idx (get-neighbourhood-coords (idx->coords node)))
            passable (filter #(and (not (contains? visited %1))
                                   ((comp not-walls? (partial nth input)) %1 )) (doall neighbours))
            new-visited (reduce conj visited passable)
            new-queue (reduce conj (pop q) passable)]

        (recur new-keys new-doors new-queue new-visited (inc n))))))

(do
  (println "starting")
  (println "========================================")
  (find-all-keys-doors main-board 82))

(def modified-board (first (modify-input main-board (find-char main-board \@))))

(defn get-ignored-keys [input pos]
  (let [[keys doors _] (find-all-keys-doors input pos)]
    [(count keys) (set/difference (set doors) (set (map #(Character/toUpperCase %) keys))) pos]))

(defn find-all-characters [input character]
  (reduce (fn [[coll idx] i-char] (if (= i-char character)
                                    [(conj coll idx) (inc idx)]
                                    [coll (inc idx)]))
          [[] 0]
          input))

(def input-positions
  (first (find-all-characters modified-board \@)))

(defn board-passable-and-ignored? [keys-taken ignored character]
  (or (= \. character) (= \@ character) (board-key? character)
      (contains? keys-taken (Character/toLowerCase character))
      (contains? ignored character)))

(defn solve-maze-2 [input keys-count ignore-doors input-idx]
  (loop [queue (queue `(~(visited-node. input-idx #{} 0)))
         visited #{}
         n 0]
    (if (or (= keys-count (count (.keys_taken (peek queue)))) (empty? queue))
      (peek queue)
      (let [node (peek queue)
            neighbours (map coords->idx (get-neighbourhood-coords (idx->coords (.index node))))
            keys-taken (.keys-taken node)
            passable (filter (comp (partial board-passable-and-ignored? keys-taken ignore-doors) (partial nth input)) neighbours)
            new-visited (conj visited node)
            new-nodes (map (fn [idx] (visited-node. idx (add-key input keys-taken idx) (inc (.steps node)))) passable)
            new-nodes-filtered (filter #(not (contains? visited %1)) new-nodes)
            new-queue (reduce #(conj %1 %2) queue new-nodes-filtered)]

        (recur (pop new-queue) new-visited (inc n))))))

(println "Part 2: "
         (let [parts (map #(get-ignored-keys modified-board %) input-positions)]
           (reduce + (map (fn [[keys-count ignore-doors input-idx]] (.steps (solve-maze-2 modified-board keys-count ignore-doors input-idx))) parts))))
