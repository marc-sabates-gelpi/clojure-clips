(ns clips.core
  (:require [clojure.core.async :as async]
            [spyscope.core]))
;;01/03/2018
doall
run!
dorun
(defn analyze [board]
  (-> '()
      (into board) ;; rows
      (into (apply map (fn [& args] args) board)) ;; columns
      (into (map (fn [coords] (map #(get-in board %) coords)) ;; diagonals
                 (let [rang (range (count board))
                       max-i (apply max rang)
                       test-diag1 #(= %1 %2)
                       test-diag2 #(= %1 (- max-i %2))]
                   (map #(for [x rang y rang :when (% x y)] [x y])
                        (list test-diag1 test-diag2)))))
      (as-> lines
          (filter #(= 1 (count (frequencies %))) lines)
        (filter #(not-any? #{:e} %) lines))
      ffirst))
(= nil (analyze [[:e :e :e :e]
                 [:e :e :e :e]
                 [:e :e :e :e]
                 [:e :e :e :e]]))

(= nil (analyze [[:e :e :e]
                 [:e :e :e]
                 [:e :e :e]]))

(= :x (analyze [[:x :e :o]
                [:x :e :e]
                [:x :e :o]]))

(= :o (analyze [[:e :x :e]
                [:o :o :o]
                [:x :e :x]]))

(= nil (analyze [[:x :e :o]
                 [:x :x :e]
                 [:o :x :o]]))

(= :x (analyze [[:x :e :e]
                [:o :x :e]
                [:o :e :x]]))

(= :o (analyze [[:x :e :o]
                [:x :o :e]
                [:o :e :x]]))

(= nil (analyze [[:x :o :x]
                 [:x :o :x]
                 [:o :x :o]]))
;;#74 Filter Perfect Squares
;; C-c RET h d [org.clojure/math.numeric-tower "0.0.4"]
(require '[clojure.math.numeric-tower :as math])
(def mem-sqrt (memoize math/sqrt))
(defn perf-sq [s]
  (as-> (clojure.string/split s #",") nums
    (transduce (comp
                (map read-string)
                (filter #(int? (mem-sqrt %)))
                (map str)
                (interpose ","))
               (completing str)
               nums)))
(int? (math/sqrt 2))
(str)

(= (perf-sq "4,5,6,7,8,9") "4,9")
(= (perf-sq "15,16,25,36,37") "16,25,36")
;; v1.4 compatible
(defn perf-sq [s]
  (-> (clojure.string/split s #",")
      ((fn [nums] (map read-string nums)))
      ((fn [nums] (filter #(let [sqrt (mem-sqrt %)] (= sqrt (int sqrt))) nums)))
      ((fn [nums] (map str nums)))
      ((fn [nums] (interpose "," nums)))
      ((fn [nums] (reduce str nums)))))
(= (perf-sq "4,5,6,7,8,9") "4,9")
(= (perf-sq "15,16,25,36,37") "16,25,36")
(def perf-sq (let [mem-perf-sq? (memoize (fn [n]
                                           (let [initial-candidates (for [x (range 1 n) :when (>= n (* x x))] x)]
                                             (loop [candidates initial-candidates found? false]
                                               (if (empty? candidates)
                                                 found?
                                                 (recur (rest candidates) (let [cand (first candidates)]
                                                                            (= n (* cand cand)))))))))]
               (fn [s]
                 (-> (clojure.string/split s #",")
                     ((fn [nums] (map read-string nums)))
                     ((fn [nums] (filter mem-perf-sq? nums)))
                     ((fn [nums] (map str nums)))
                     ((fn [nums] (interpose "," nums)))
                     ((fn [nums] (reduce str nums)))))))
