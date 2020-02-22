(ns clips.aoc.2019.day3-attempt-2
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clips.aoc.2019.day3 :as day3]))

(defn make-segment
  [acc el]
  (conj acc (-> (last acc)
                (or {:b {:x 0 :y 0}})
                (day3/make-directed-segment el))))

(defn- thread-last-split
  [pattern s]
  (str/split s pattern))

(defmulti segment->points :dir)

(defmethod segment->points :u
  [{{x :x ay :y} :a {by :y} :b}]
  ;; The range doesn't get the ends (only inner points)
  (for [y (range (inc ay) by)]
    [x y]))

(defmethod segment->points :d
  [{{x :x ay :y} :a {by :y} :b}]
  ;; The range doesn't get the ends (only inner points)
  (for [y (range (inc by) ay)]
    [x y]))

(defmethod segment->points :r
  [{{ax :x y :y} :a {bx :x} :b}]
  ;; The range doesn't get the ends (only inner points)
  (for [x (range (inc ax) bx)]
    [x y]))

(defmethod segment->points :l
  [{{ax :x y :y} :a {bx :x} :b}]
  ;; The range doesn't get the ends (only inner points)
  (for [x (range (inc bx) ax)]
    [x y]))

(defn part1
  ([] (part1 (slurp "resources/aoc2019/day3")))
  ([s]
   (->> s
        (thread-last-split #",")
        (reduce make-segment [])
        (mapcat segment->points)
        frequencies
        (filter (comp (partial <= 2) val))
        (map key)
        (sort-by (partial day3/d first last [0 0]))
        first)))

;(part1)
;=> [317 0]
;That's not the right answer; your answer is too low.

; It is wrong because it looks for intersections on only the first wire..

