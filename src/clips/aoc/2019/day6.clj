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

;;;; Part 2
(defn part2 [s]
  (let [orb-map (->> (if (string/starts-with? s "resources/")
                       (slurp s)
                       s)
                     string/split-lines
                     (map #(string/split % #"\)"))
                     (map (partial map (comp keyword string/lower-case)))
                     orbit-sort
                     (reduce (fn [acc [orbited in-orbit]]
                               (when-not (get acc orbited) (throw (Exception. (format "[ %s ] Object %s hasn't been defined yet!" in-orbit orbited))))
                               (assoc acc in-orbit {:num  (inc (get-in acc [orbited :num]))
                                                    :tree (conj (get-in acc [orbited :tree]) in-orbit)}))
                             {:com {:num  0
                                    :tree [:com]}}))
        [san you :as paths] (map :tree (vals (select-keys orb-map [:san :you])))
        common  (conj (set (filter (set san) you)) :you :san)]
    (apply + (map (comp count (partial remove common)) paths))))

;(part2 "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")
;=> 4
;(part2 "resources/aoc2019/day6")
;=> 472
