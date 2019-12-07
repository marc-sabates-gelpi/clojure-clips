(ns clips.aoc.2019.day1
  (:require [clojure.string :refer [split-lines]]
            [clojure.edn :as edn]))

;;;; Session 07/12/2019
;; AoC 2019 Day 1 Part 1

(->> "resources/aoc2019/day1"
     slurp
     split-lines
     (transduce (comp
                  (map edn/read-string)
                  (comp
                    (map #(/ % 3))
                    (map int)
                    (map #(- % 2))))
                +))
;; => 3560353

;; AoC 2019 Day 1 Part 2

;; Fuel formula: f(x) = round-down (x / 3) - 2
;; The reverse calculation is: fr (y) = (y + 2) * 3
(fn [y] (-> y
            (+ 2)
            (* 3)))
;; min weight that gets fuel is: fr (0)
(letfn [(fr [y] (-> y
                    (+ 2)
                    (* 3)))]
  (fr 0))
;; result: 8, when mass is <= 8 the fuel will be 0
;; A procedural way (1) of solving the problem would be to run a loop until the current delta mass is <= 8.
;; A functional way (2) would be to create a lazy sequence starting on the modules fuel and calculating
;; the fuel for the new mass until 0 and add it up.
;; A mathematical way (3) would be to come up with a series and look up for its series sum or calculate the
;; lim of the fuel formula.

;; 2) Functional way:
(defn fuel-for
  "Return the fuel needed for a certain mass.
  The fuel formula is: f(x) = round-down (x / 3) - 2."
  [mass]
  (-> mass
      (/ 3)
      int
      (- 2)))

(defn fuel-series
  "Return a collection of fuel to carry the previous fuel."
  [fuel]
  (let [curr (fuel-for fuel)]
    (when (pos? curr)
      (lazy-seq (cons curr (fuel-series curr))))))

(->> "resources/aoc2019/day1"
     slurp
     split-lines
     (transduce (comp
                  (map edn/read-string)
                  (map #(reduce + (fuel-series %))))
                +))
;=> 5337642
