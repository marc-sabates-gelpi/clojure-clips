(ns clips.aoc.2019.day4
  (:require [clojure.edn :as edn]))

(defn monotonically-inc
  [s]
  (->> s
       (re-seq #"\d")
       (map edn/read-string)
       sort
       (apply str)
       (= s)))

(defn- truthy?
  [v]
  (if v true false))

(defn comply
  [rules x]
  (let [s (pr-str x)]
    (every? truthy? (map #(% s) rules))))

(defn part1
  []
  (count
    (for [x (range 284639 (inc 748759)) :when (comply [monotonically-inc
                                                       (partial re-find #"(\d)\1")]
                                                      x)]
     x)))

;(part1)
;=> 895

(defn part2
  []
  (count
    (for [x (range 284639 (inc 748759)) :when (comply [monotonically-inc
                                                       #(some (comp #{2} count) (partition-by identity %))]
                                                      x)]
      x)))

;(part2)
;=> 591
