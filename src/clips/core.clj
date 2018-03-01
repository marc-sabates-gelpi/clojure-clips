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
