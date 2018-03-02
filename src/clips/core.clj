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
;;02/03/2018
(def k (keyword "two words"))
(str k)
(def m {k "value"})
(prn m)
(get m :two\spacewords)
(reduce + '(nil))
(reduce (fnil + 0) [nil])
(+ nil)
(reduce + (map identity [nil]))
;;#75 Euler's Totient Function
(defn gcd [a b]
  (letfn [(gcd- [a b]
             (let [modulo (mod a b)]
               (if (= modulo 0)
                 b
                 (gcd- a modulo))))]
    (gcd- (max a b) (min a b))))
(gcd 3 5)
(defn eulers-totient [n]
  (letfn [(gcd [a b]
            (letfn [(gcd- [a b]
                      (let [modulo (mod a b)]
                        (if (= modulo 0)
                          b
                          (gcd- a modulo))))]
              (gcd- (max a b) (min a b))))
          (co-prime? [a b]
            (= 1 (gcd a b)))]
    (cond
      (= 1 n) 1
      :else (count (filter (partial co-prime? n) (range 1 n))))))
(eulers-totient 1)
	
(= (eulers-totient 1) 1)
(= (eulers-totient 10) (count '(1 3 7 9)) 4)
(= (eulers-totient 40) 16)
(= (eulers-totient 99) 60)

(defn co-prime? [a b]
            (= 1 (gcd a b)))
(count (map (partial (partial gcd 40)) (range 1 40)))
deref
future
iterate
(let [m {:a [\a] :b [\b] :another-a [\a] :a-bis [\a] :c [\c]}] (clojure.set/index m (keys m)))
(let [m {:a [\a] :b [\b] :another-a [\a] :a-bis [\a] :c [\c]}] (clojure.set/index m [key]))
(let [m #{{:a [\a]} {:b [\b]} {:another-a [\a]} {:a-bis [\a]} {:c [\c]}}] (clojure.set/index m [key]))
(gcd 40 6) ;; => 4! This is completely wrong! mu gcd is wrong!
(defn gcd [a b]
  (letfn [(gcd- [a b]
             (if (= b 0)
               a
               (gcd- b (mod a b))))]
    (gcd- (max a b) (min a b))))
(gcd 40 6) ;; => 2! Ahhh! That's better!
(defn eulers-totient [n]
  (letfn [(gcd [a b]
            (letfn [(gcd- [a b]
                      (if (= b 0)
                        a
                        (gcd- b (mod a b))))]
              (gcd- (max a b) (min a b))))
          (co-prime? [a b]
            (= 1 (gcd a b)))]
    (cond
      (= 1 n) 1
      :else (count (filter (partial co-prime? n) (range 1 n))))))
(= (eulers-totient 1) 1)
(= (eulers-totient 10) (count '(1 3 7 9)) 4)
(= (eulers-totient 40) 16)
(= (eulers-totient 99) 60)
