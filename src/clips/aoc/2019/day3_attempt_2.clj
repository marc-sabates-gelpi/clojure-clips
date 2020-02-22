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
  (for [y (range ay (inc by))]
    [x y]))

(defmethod segment->points :d
  [{{x :x ay :y} :a {by :y} :b}]
  (for [y (reverse (range by (inc ay)))]
    [x y]))

(defmethod segment->points :r
  [{{ax :x y :y} :a {bx :x} :b}]
  (for [x (range ax (inc bx))]
    [x y]))

(defmethod segment->points :l
  [{{ax :x y :y} :a {bx :x} :b}]
  (for [x (reverse (range bx (inc ax)))]
    [x y]))

(defn part1
  "Note: It is wrong because it looks for intersections assuming there was only one wire.."
  ([] (part1 (slurp "resources/aoc2019/day3")))
  ([s]
   (->> s
        (thread-last-split #",")
        (reduce make-segment [])
        (map segment->points)
        (mapcat (comp butlast rest))
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
       (map segment->points)
       (mapcat (comp butlast rest))
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

(defn wire->all-points
  [s]
  (->> s
       (thread-last-split #",")
       (reduce make-segment [])
       (mapcat segment->points)
       dedupe
       rest))

(defn steps
  [[wire1 wire2 :as _wires-points] inter]
  (let [steps-w1 (map (comp inc count #(take-while (complement #{%}) wire1)) inter)
        steps-w2 (map (comp inc count #(take-while (complement #{%}) wire2)) inter)]
    (map + steps-w1 steps-w2)))

(defn part2
  ([] (part2 (slurp "resources/aoc2019/day3")))
  ([s]
   (let [wires               (str/split-lines s)
         wires-points        (map wire->all-points wires)
         intersections       (->> wires
                                  (mapcat wire->points)
                                  frequencies
                                  (filter (comp (partial <= 2) val))
                                  (map key))
         steps-intersections (steps wires-points intersections)]
     (first (sort-by first (map vector steps-intersections intersections))))))

;(part2 "R8,U5,L5,D3\nU7,R6,D4,L4")
;=> [30 [6 5]]
;(part2 "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")
;=> [610 [158 -12]]
;(part2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
;=> [410 [107 47]]
;(part2)
;=> [6484 [-277 -1068]]
