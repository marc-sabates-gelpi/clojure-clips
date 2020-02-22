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
  "Note: It is wrong because it looks for intersections assuming there was only one wire.."
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

(defn wire->points
  [s]
  (->> s
       (thread-last-split #",")
       (reduce make-segment [])
       (mapcat segment->points)
       set))

(def ^:private p+d (juxt identity (partial day3/d first last [0 0])))

(defn part1'
  ([] (part1' (slurp "resources/aoc2019/day3")))
  ([s]
   (->> s
        str/split-lines
        (mapcat wire->points)
        frequencies
        (filter (comp (partial <= 2) val))
        (map key)
        (sort-by (partial day3/d first last [0 0]))
        first
        p+d)))

;(part1' "R8,U5,L5,D3\nU7,R6,D4,L4")
;=> [[3 3] 6]
;(part1' "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")
;=> [[155 4] 159]
;(part1' "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
;=> [[124 11] 135]
;(part1')
;=> [[-34 -352] 386]
