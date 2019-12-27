(ns day_14.day14
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            ))
(use 'clojure.test)

(load-file "../util/simple2D.clj")
(clojure.core/alias 's2D 'util.simple2D)


(defn trim-coll [coll]
  (map str/trim coll))

(defrecord ComponentPart [name amount])


(defn split-by-sequence [sequence s]
  (-> s
      (str/split sequence)
      (trim-coll)))

(def input (->> (slurp "input.txt")
                (str/trim)))

(def test-input "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(defn split-component [component]
  (->> component
       (split-by-sequence #" ")
       (apply (fn [val name] (->ComponentPart name (read-string val))))))

(defn split-components [coll]
  (->> coll
       (split-by-sequence #",")
       (map split-component)))

(defn split-equation [equation]
  (let [re (str/split equation #"=>")
        left (split-components (first re))
        right (split-components (second re))]
  {:left left :right right}))


(split-equation "22 RHDZ, 22 DZGWQ, 2 NGJRN, 14 XHRWR, 21 VWPMZ => 8 BPHZ")

(defn split-input-set [coll]
  (->> coll
       str/split-lines
       (map split-equation)))

(split-input-set input)

(def test2
"10 ORE => 3 A
10 ORE, 4 A => 2 B
1 A, 5 B => 1 FUEL")

(def requirement-part-1 {"FUEL" 1})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-equations-map [equations]
  (reduce
   (fn [coll equation]
     (let [right (first(:right equation))]
       (assoc coll (:name right) equation)
       ))
   {}
   equations))

(defn find-equation-map [eq-map name]
  (get eq-map name))

(make-equations-map (split-input-set test2))

(defn update-stock [stock [name amount]]
  (update stock name (fn [old] (if old
                                  (+ old amount)
                                  amount))))

;; (update-stock {} ["AA" 55])

(defn number-of-equations-needed [one-eq-makes needs]
  (let [multiplier (bigint (/ needs one-eq-makes))]
    multiplier))

(number-of-equations-needed 5 1)

(defn multiply-equations [equations multiplier]
  (map
   (fn [eq]
     (->ComponentPart (:name eq) (* multiplier (:amount eq))))
   equations))

(multiply-equations (:left (second (first (make-equations-map (split-input-set input))))) 13)

(defn take-from-stock [stock name amount]
  (let [in-stock (get stock name)
        new-val (if in-stock  (max (- in-stock amount) 0) 0)
        taken (if in-stock (max (- amount in-stock) 0) amount)]
    [(update stock name (fn [old]
                         new-val)) taken]))

(take-from-stock {"A" 3} "A" 5)

(defn build-component [equations-map stock [req-name required-amount]]
  (let [[new-stock new-req-amount] (take-from-stock stock req-name required-amount)]
    (if (> new-req-amount 0)
      (let [equation (find-equation-map equations-map req-name)
            eq-amount (:amount (first (:right equation)))
            multiplier (number-of-equations-needed eq-amount new-req-amount)
            multiplier (if (> (mod new-req-amount eq-amount) 0) (inc multiplier) multiplier)
            left-over (- (* multiplier eq-amount) new-req-amount)
            multiplied-equations (multiply-equations (:left equation) multiplier)
            new-stock (update-stock new-stock [req-name left-over])]
        [new-stock multiplied-equations])
      (do
        [new-stock '()]
        )
      )))

(defn filter-name [name coll]
  (filter
   (fn [[k v]] (not= k name))
   coll))

(defn find-non-name [name coll]
  (loop [part-coll coll]
    (if (empty? part-coll)
      nil
      (if (not= (first (first part-coll)) name)
        (first part-coll)
        (recur (rest part-coll))))))

(defn find-non-ore [coll]
  (find-non-name "ORE" coll))

(find-non-ore {"ORE" 5, "CAA" 5, "BBB" 558})

(defn merge-with-equations [to-expand equations]
  "to-expand is map, equations is list of equations"
  (reduce (fn [coll eq] (update-stock coll [(:name eq) (:amount eq)]))
          to-expand
          equations))

;; TODO: naming!
(defn find-solution [equations-map to-expand]
  (loop [stock {}
         expandable to-expand]
    (let [non-ore (find-non-ore expandable)]
      (if (not (nil? non-ore))
        (let [[name amount] non-ore
              [new-stock new-parts] (build-component equations-map stock [name amount])
              new-requirements (merge-with-equations (dissoc expandable name) new-parts)]
          (recur new-stock new-requirements))
        [stock expandable]
      ))))

(time
 (println "Part 1:"
          (find-solution (make-equations-map (split-input-set input)) requirement-part-1)))

;; result 371695

(def trillion-ore 1000000000000)

(def one-fuel-needs-ore 371695)

(def trilion-ore-to-fuel-low-estimate (/ trillion-ore one-fuel-needs-ore))

(def requirement-part-2 {"FUEL" 1})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-fuel-for-ore [ore]
  (loop [low-estimate (/ ore one-fuel-needs-ore)
         upper-estimate (* 2 low-estimate)
         n 0]
    (let [half (* 0.5 (+ low-estimate upper-estimate))
          [stock result] (find-solution (make-equations-map (split-input-set input)) {"FUEL" half})]
      (cond
        (> n 100) half
        (= (get result "ORE") ore) half
        (> (get result "ORE") ore) (recur low-estimate half (inc n))
        (< (get result "ORE") ore) (recur half upper-estimate (inc n))
        ))))

(time
 (println "Part 2: "
          (find-fuel-for-ore trillion-ore)))
;; 4052920


;; (java.util.ArrayList. [1 2 3])
