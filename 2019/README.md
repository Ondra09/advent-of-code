# Clojure Advent Of Code

## Optimization hints
https://tech.redplanetlabs.com/2020/09/02/clojure-faster/
https://www.youtube.com/watch?v=TW1ie0pIO_E

-- functions
- use multiple arity functions instead of variadic ones
- do not use partial
- use (seq []) instead of (empty? [])
- keywoard equality (= :a :a) bad; (keyword-identical? :a :a) better

-- runtime polymorphism
- bad
 ```clojure
 (defmulti foo (fn [& args] ...))
 ```
- Good, but less elegant use cond or condp or manualy dispatch
```clojure
(condp = x
 ... )
 ```
- Fastest??
```clojure
 (defprotocol Fooable
   (foo [this x]))
 (deftype Thing
   Fooable
   (foo [this x]...))
```
- multimethods too slow for tight loops
- condp complies to conditional that can be optimized
- protocols and types are fastest but static
- tradeoff between dynamic code vs. speed

- if function is used multiple times with same result just def it
```clojure
 (defn foo [x]
   (conj (range 10) x))
```
- good skip extra calls
 ```clojure
 (def numbers (range 10))
 (defn foo [x]
   (conj numbers x))
```
- use array when you need to fastly append items into collection (mutable)

-- Lazyness
- clojure have lazy seqs map/for/concat/filter
- game code is eager and inputs are finite we do not care about lazyness
- Favor reduce over map
- Write own for-loop that does not use lazy-seq to accumulate results
- lazy seqs have GC impact

- Avoid intermediat collections


- Bad, procedurally building up multiple collections
```clojure
(let [v1 (map inc numbers)
      v2 (filter pos? v1)
      v3 (map #(* 2 %) v2)]
   (into {} (map #(vector % (inc %)) v3)))
```

- Good, do it all in one shot using transients
```clojure
(loop [nums numbers
       accum (transient {})]
  (let [n (first numbers) ]
    (if n
      (recur (rest nums) (assoc! accum n (inc n)))
      (persisten! accum))))
```
- or use java/java-script array

-- GC is quite good in JS engines 7-10% time
- use typehint ^boolean
- Use arrays and transient
- advanced compiler options
-   :static-fns
-   :elide-assert

- use Interop with the host environment if needed

- Use loop instead of for or into with transients or arrays as the accumulator
- Avoid boxing and unboxing i.e multiple maps/fors over a collection, use transducers, reducers or loops
-- https://medium.com/formcept/performance-optimization-in-clojure-using-reducers-and-transducers-a-formcept-exclusive-375955673547
- Don't make multiple calls to get the same data, put it in a let
- Avoid heavily nested closures as the lookup tree becomes very long and slow
- Favor eager operations over lazy operations i.e reduce instead of for or map
- Don't use concat or mapcat as they can be slow and generate lots of garbage
- Don't use last as it will need to traverse the whole sequence, use nth instead if you know how many elements are in the collection
- Don't use hashmaps as functions ({:a 1} :a), instead use get or keywords as functions
- Always return the same type from a function (V8 can then optimize it)

# Algorithms implementatio
https://rosettacode.org/wiki
