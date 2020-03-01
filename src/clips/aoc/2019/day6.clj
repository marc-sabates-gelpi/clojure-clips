(ns clips.aoc.2019.day6
  (:require [clojure.string :as string]
            [taoensso.timbre :refer [spy debug]]))

(defn orbit-sort
  "Return a sorted coll.
  Sort by current orbited needs to exist in the partially sorted coll.
  NOTE: This is highly un-efficient.. needs improvement."
  [coll]
  (loop [sorted [] to-sort (vec coll)]
    (if (seq to-sort)
      (let [[orbited _ :as current] (first to-sort)]
        (if (or (some (comp #{orbited} second) (reverse sorted)) (= :com orbited))
          (recur (conj sorted current) (vec (rest to-sort)))
          (recur sorted (conj (vec (rest to-sort)) current))))
      sorted)))

(defn part1 [s]
  (->> (if (string/starts-with? s "resources/")
         (slurp s)
         s)
       string/split-lines
       (map #(string/split % #"\)"))
       (map (partial map (comp keyword string/lower-case)))
       #_shuffle
       orbit-sort
       (reduce (fn [acc [orbited in-orbit]]
                 (when-not (get acc orbited) (throw (Exception. (format "[ %s ] Object %s hasn't been defined yet!" in-orbit orbited))))
                 (assoc acc in-orbit {:num  (inc (get-in acc [orbited :num]))
                                      :tree (conj (get-in acc [orbited :tree]) in-orbit)}))
               {:com {:num  0
                      :tree [:com]}})
       vals
       (map :num)
       (apply +)))

;(part1 "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")
;=> 42
;(part1 "resources/aoc2019/day6")
;=> 308790
