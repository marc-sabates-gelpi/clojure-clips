(ns clips.core
  (:require [clojure.core.async :as async]
            [spyscope.core]))
;;; Session 01/04/2018

(reduce + 0 (range 10))

(def ^:private some-primes '(3 5 7 11 13 17 19 23))

(defn- my-processor-f
  "Function that processes an element to a collection."
  [x]
  (when (< x 5)
    (map #(* % x) some-primes)))

(defn- my-reduce-f
  "Function that reduces to itself."
  [acc el]
  (printf "Accumulated: %s. Current element: %s\n" acc el)
  (reduce my-reduce-f (conj acc el) (my-processor-f el)))

(reduce my-reduce-f #{} (my-processor-f 1))


