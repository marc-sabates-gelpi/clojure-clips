(ns clips.core
  (:require [clojure.core.async :as async]
            [spyscope.core]
            [clj-memory-meter.core :as mm]
            [clojure.string :as string]))

;;; AoC 2016 Day 9 Part 1
(defn uncompress
  [in]
  (if-not (= \( (first in))
    [(subs in 1 (count in)) (subs in 0 1)]
    (let [cmd (re-find #"\([0-9]+[Xx][0-9]+\)" in)
          [len times] (map clojure.edn/read-string (-> cmd
                                                       (string/replace #"[\(\)]" "")
                                                       (string/split #"[Xx]")))
          remaining-in (subs in (+ (count cmd) len) (count in))
          pattern (subs in (count cmd) (+ (count cmd) len))
          out (apply str (take (* len times) (cycle pattern)))]
      [remaining-in out])))

(defn day9-1
  [in]
  (let [input (if (string/index-of in "resources/") (slurp in) in)]
    (loop [compressed input uncompressed nil]
      (if (seq compressed)
        (let [[updated-compressed new-chunk] (uncompress compressed)]
          (recur updated-compressed (str uncompressed new-chunk)))
        uncompressed))))

(defmacro str-and-count
  [form#]
  `((juxt identity count) ~form#))

(str-and-count (day9-1 "ADVENT"))
(str-and-count (day9-1 "A(1x5)BC"))
(str-and-count (day9-1 "(3x3)XYZ"))
(str-and-count (day9-1 "A(2x2)BCD(2x2)EFG"))
(str-and-count (day9-1 "(6x1)(1x3)A"))
(str-and-count (day9-1 "X(8x2)(3x3)ABCY"))
(str-and-count (day9-1 "resources/aoc2016/day9"))
