(ns day_18.day18
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

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

;;(def input test-input-3)

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

(def get-xy (get-xy-board  (row-length input)))
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
    (if-let [idx (if (= -1 j-idx) nil j-idx)]
      (idx->coords idx)
      nil)))

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

(defn board-key? [character]
  (Character/isLowerCase character))

(defn board-gate? [character]
  (Character/isUpperCase character))

(defn board-passable? [character] ;; add found keys
  (or (= \. character) (= \@ character)))

(defn not-walls? [character]
  (not= \# character))

(defn find-nearest-keys [board loc]
  (loop [to-search `(~loc)
         found '()
         visited #{}
         n 0]
    (if (or (empty? to-search) (= n 10000))
      found
      (let [node (first to-search)
            ;; ff (println "get-xy " (count board) node)
            node-val (apply get-xy board node)
            found (if (board-key? node-val) (conj found node-val) found)
            neighbourhood-coords (if (board-passable? node-val) (get-neighbourhood-coords node) [])
            to-search (reduce #(if (get visited %2) %1 (conj %1 %2)) (rest to-search) neighbourhood-coords)
            visited (conj visited node)
            ]
        (recur to-search found visited (inc n)))
      )))

(defn assoc-if-idx-not-nil [coll idx val]
  (if-not (nil? idx)
    (assoc coll idx val)
    coll))

(defn unlock-door [board key]
  "removes both key & doar from board"
  (let [key-pos (coords->idx (find-char board key))
        door-idx (find-char board (char (- (int key) 32)))
        door-pos (when door-idx (coords->idx door-idx))
        ]
    (-> board
        (assoc-if-idx-not-nil key-pos \.)
        (assoc-if-idx-not-nil door-pos \.))))

(defn find-all-keys [board]
  (reduce (fn [coll character] (if (or (= \@ character) (board-key? character)) (conj coll character) coll)) '() board))

(defn compute-all-distance-maps-for-keys [board]
  (let [keys  (find-all-keys board)]
    (reduce (fn [coll key]
              (let [key-coord (find-char board key)]
                (assoc coll key (distance-map board not-walls? key-coord)))) {} keys)))

(def ^:dynamic *minimum* [99999999 false] )

(defn remove-all-doors [board character character-coord dst-maps dst-so-far]
  (let [keys (find-nearest-keys board character-coord)
        curr-distance-map (get dst-maps character)]
    (if (or (empty? keys)
            (> dst-so-far 4850))
      (when (> (first *minimum*) dst-so-far)
            (alter-var-root #'*minimum* (constantly [dst-so-far (empty? keys)] ))
            )
      (doseq [key keys]
        (let [key-coord (find-char board key)
              dst (get curr-distance-map key-coord)]
          (remove-all-doors (unlock-door board key) key (find-char board key) dst-maps (+ dst-so-far dst))
;;          (for [tail (remove-all-doors (unlock-door board key) key (find-char board key) dst-maps (+ dst-so-far dst))]
            ;;(cons [key dst] tail)cider
            )))))



(alter-var-root #'*minimum* (constantly [9999999999 0]) )

(remove-all-doors main-board \@ (find-char main-board \@) (compute-all-distance-maps-for-keys main-board) 0)


(println "Resutl 1: " *minimum*)
;; 4702 not correct
;; 4802 not correct
;; 4952 not correct
;; 4816 not correct
(comment
 (println "Part 1: "
          (->> (remove-all-doors main-board \@ (find-char main-board \@) (compute-all-distance-maps-for-keys main-board) 0)
               (map (fn [coll] (reduce (fn [dst [k val]] (+ dst val)) 0 coll)) )
               (apply min))))
