(ns clips.aoc.2019.day7
  (:require [clips.aoc.2019.day5 :as day5]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [taoensso.timbre :refer [spy debug]]))

;(do (day5/part2 "resources/aoc2019/day7") nil)
;$ Input: 0
;$ Input: 0
;3
;=> nil

;(for [a (range 5) b (range 5) c (range 5) d (range 5) e (range 5) :when (= (list a b c d e) (distinct (list a b c d e)))]
;  [a b c d e])

;(with-out-str (with-in-str "0\n0" (do (day5/part2 "resources/aoc2019/day7") nil)))
;=> "$ Input: $ Input: 3\n"

;(last (re-find #"\$\sInput:\s\$\sInput:\s([0-9]+)\s" (with-out-str (with-in-str "0\n0" (do (day5/part2 "resources/aoc2019/day7") nil)))))
;=> "3"

(defn run-with
  "Return the output of running `code` with given `phase` and `input`."
  [code phase input]
  (last
    (re-find #"\$\sInput:\s\$\sInput:\s([0-9]+)\s"
             (with-out-str
               (with-in-str (format "%s\n%s" phase input)
                            (do (day5/part2 code) nil))))))

;(run-with (mapv edn/read-string (-> "resources/aoc2019/day7" slurp (string/split #","))) "0" "0")
;=> "3"

(defn run-series
  [code series]
  (loop [left series input nil]
    (if (seq left)
      (let [phase (first left)]
        (recur (rest left) (run-with code phase (or input "0"))))
      [(edn/read-string input) series])))

(defn part1
  []
  (let [code (mapv edn/read-string (-> "resources/aoc2019/day7" slurp (string/split #",")))
        perm (for [a (range 5) b (range 5) c (range 5) d (range 5) e (range 5) :when (= (list a b c d e) (distinct (list a b c d e)))]
               (mapv str [a b c d e]))]
    (->> perm
         (map (partial run-series code))
         (sort-by first)
         last)))

;(part1)
;=> [17440 ["2" "4" "3" "0" "1"]]
