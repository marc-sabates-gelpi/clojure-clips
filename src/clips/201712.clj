(ns clips.core
  (:require [clojure.core.async :as async]))
;;23/11/2017 
(sort '(3 2 1))
(sort [3 2 1])
(def my-sorted? #(let [coll (seq %)] (= (sort coll) coll)))
(sorted? '(1 1 1))
(my-sorted? '(1 1 1))
(my-sorted? '(1 2 3))
(my-sorted? '(1 3 2))
(my-sorted? "abc")
(sort "abc")
(sort 1)
(my-sorted? "acb")
(my-sorted? nil)
;;27/11/2017
(group-by :a [{:a "a" :b 1} {:a "a" :b 2} {:a "b" :b 1}])
;;28/11/2017
(derive ::pdf ::format)
(derive ::xml ::format)
(seq [1 2 3])
(empty nil)
(empty (butlast [{:month "2015-01" :consumption 0}]))
(into [] '(1 2 3))
(conj [] '(1 2 3))
(into (empty []) '(1 2 3))
(into (empty '()) '(1 2 3))
(take 10 (lazy-seq (conj '() 1)))
(take 10 (repeatedly (fn [] 1)))
(next '(1 2 3))
(rest '(1 2 3))
;;29/11/2017
(#(cond
     (not-empty (keys %)) :map
     (= (conj % (first %)) %) :set) #{1 2})
(not-empty (keys #{1 2}))
(first '())
(conj '() nil)
(conj [] (first []))
(conj {} (first {}))
(first {})
(conj {} nil)
(= (empty {}) '())
(= (empty {}) {})
(= (empty {}) #{})
(= (empty {}) [])
(#(cond
    (= (empty %) {}) :map
    (= (empty %) #{}) :set
    :else (cond (let [test (into % '(-1 -2))] (= (first test) (peek test))) :list
                :else :vector)) [])
(= (empty []) '())
(= (empty '()) [])
(first '(1 2 3))
(first [1 2 3])
(peek)
(def black-box-testing #(cond
                          (= (empty %) {}) :map
                          (= (empty %) #{}) :set
                          :else (cond (let [test (into % '(-1 -2))] (= (first test) (peek test))) :list
                                      :else :vector)))
(black-box-testing (range (rand-int 20)))
(black-box-testing [1 2 3 4 5 6])
(black-box-testing #{10 (rand-int 5)})
(map black-box-testing [{} #{} [] ()])
(def black-box-testing #(cond
                          (= (empty %) {}) :map
                          (= (empty %) #{}) :set
                          :else (cond (let [vss2 "very-specific-string-2" test (into % (list "very-specific-string-1" vss2))] (= (first test) vss2)) :list
                                      :else :vector)))
(mod 2 4)
(mod 2 2)
(mod 10 5)
(mod 7 5)
(mod 7 2)
(mod 7 1)
(defn gcd1 [a b]
  (let [modulo (mod a b)]
    (if (= modulo 0)
      b
      (gcd1 a modulo))))
(defn gcd [a b]
  (if (> a b)
    (gcd1 a b)
    (gcd1 b a)))
(gcd 2 4)
(gcd 4 2)
(gcd 5 10)
(gcd 5 7)
(gcd 1023 858)
(gcd1 4 2)
(gcd1 2 4)
(gcd1 858 1023)
(#((fn gcd [a b]
      (let [modulo (mod a b)]
        (if (= modulo 0)
          b
          (gcd a modulo)))) (max %1 %2) (min %1 %2)) 858 1023)
(max 1 2)
(min 1 2)
(defn prime? [n]
  (and
   (not= n 1)
   (= ((fn smallest-divisor [n test]
         (cond
           (> (* test test) n) n
           (= (mod n test) 0) test
           :else (smallest-divisor n (inc test)))) n 2) n)))
(prime? 1)
(prime? 2)
(prime? 4)
(map prime? (range 1 11))
(#(and
   (not= % 1)
   (= ((fn smallest-divisor [n test]
         (cond
           (> (* test test) n) n
           (= (mod n test) 0) test
           :else (smallest-divisor n (inc test)))) % 2) %)) 4)
(#(first (filter (fn [n]
                    (and
                     (not= n 1)
                     (= ((fn smallest-divisor [n test]
                           (cond
                             (> (* test test) n) n
                             (= (mod n test) 0) test
                             :else (smallest-divisor n (inc test)))) n 2) n))) %)) [1 4 3])
(some prime? [1 2])
(first (filter prime? [1 2]))
(defn next-prime [n]
  (loop [nn (inc n)]
    (if (prime? nn)
      nn
      (recur (inc nn)))))
(next-prime 1)
(next-prime 101)
(take 10 (iterate next-prime 2))
(#(take % (iterate (fn [n]
                      (loop [nn (inc n)]
                        (if ((fn [n]
                               (and
                                (not= n 1)
                                (= ((fn smallest-divisor [n test]
                                      (cond
                                        (> (* test test) n) n
                                        (= (mod n test) 0) test
                                        :else (smallest-divisor n (inc test)))) n 2) n))) nn)
                          nn
                          (recur (inc nn))))) 2)) 10)
;;02/12/2017
(def m '(1 2 3))
(#(set! (identity %) 2) m)
(var m)
(symbol 'm)
(var (identity m))
(defmacro not-nil? [x]
  `(not (nil? ~x)))
(defmacro get-first-non-empty [f coll]
  "Looks for the first element that has output when calling f on it and returns the output"
  `(loop [coll# ~coll]
     (let [elem# (first coll#)]
       (if (not-nil? elem#)
         (let [res# (~f elem#)]
           (if (not-nil? res#)
             res#
             (recur (next coll#))))))))
(true? 2)
(get-first-non-empty #(if (= % 3) (list "BINGO!")) (range 5))
(next '(1 2))
(defmacro get-first-non-empty [f coll]
  "Returns the first non-nil output of calling f"
  `(loop [coll# ~coll]
     (if (empty? coll#)
       nil
       (let [res# (~f (first coll#))]
         (if (not-nil? res#)
           (list (first coll#) res#)
           (recur (next coll#)))))))
(map #(do % nil) (range 5))
(defn filtering-transform
  [predicate]
  (fn [reducingf]
    (fn [acc item]
      (if (predicate item)
        (reducingf acc item)
        acc))))
(defn mapping-transform
  [mapf]
  (fn [reducingf]
    (fn [acc item]
      (reducingf acc (mapf item)))))
(defn my-filtering-transform
  [predicate reducingf]
  (fn [acc item]
    (if (predicate item)
      (reducingf acc item)
      acc)))
(defn my-mapping-transform
  [mapf reducingf]
  (fn [acc item]
    (reducingf acc (mapf item))))
(reduce ((filtering-transform even?) ((mapping-transform inc) +)) 0 (range 0 10))
(reduce (my-filtering-transform even? (my-mapping-transform inc +)) 0 (range 0 10))
(def xf (comp
         (filter even?)
         (map inc)))
(transduce xf + 0 (range 0 10))
(sequence xf (range 0 10))
(def inc-numbers
  (comp
   (filter number?)
   (map inc)))
(sequence inc-numbers '(a 1 2 b 3 c d 4))
(def first-non-nil
  (comp
   (map #(if (= % 3) (list % "BINGO!")))
   (remove nil?)
   (take 1)
   cat))
(sequence first-non-nil (range 5))
(into [] first-non-nil (range 5))
(transduce first-non-nil conj '() (range 5))
;;03/12/2017
(sequence (comp (keep #(if (= % 3) (list % "BINGO!"))) (take 1)) (range 5))
(sequence (keep #(if (not= % 3) (list % "BINGO!"))) (range 5))
(comp)
;;06/12/2017
(mapcat #(list (inc %)) (range 5))
(map inc (range 5))
(concat (range 5))
(apply concat (range 5))
(sequence 1)
(seq 1)
(list 1)
(doseq [n (seq (range 5))] (prn nq))
(clojure.lang.RT/iter (range 5))
(name "test")
(name :test)
(let [test "test"] (name test))
(= (name "test") (name :test) (let [test "test"] (name test)))
(some-> (if (< 0.5 (rand)) "dummy text") .toString)
(when-let [a-string (if (< 0.5 (rand)) "dummy text")] (.toString a-string))
(#(inc 3))
;;07/12/2017
(letfn [(desc [a & {:as opts}]
          {:a a :opts opts})]
  (desc 1 2 3 :max 3 :while "text"))
(letfn [(desc [a & {:as opts}]
          {:a a :opts opts})]
  (desc 1 2 :max 3 :while "text")) ;; Exception
(letfn [(desc [a & [:as opts]]
          {:a a :opts opts})]
  (desc 1 2 3 :max 3 :while "text"))
(letfn [(desc [a & [:as opts]]
          {:a a :opts opts})]
  (desc 1 2 :max 3 :while "text"))
(filter #{2} (range 5))
(= (/ (/ 120 3) 4) (/ (/ 120 4) 3))
(= (/ (/ 11 3) 4) (/ (/ 11 4) 3))
(constantly)
(defn- enforce-max-health
  [{:keys [name health]}]
  (fn [character-data]
    (or (<= (:health character-data) health)
        (throw (IllegalStateException. (str name " is already at max health!"))))))
(enforce-max-health "me" 222) ;; Wrong! That's not a map!
(defn- enforce-max-health
  [name health]
  (fn [character-data]
    (or (<= (:health character-data) health)
        (throw (IllegalStateException. (str name " is already at max health!"))))))
(enforce-max-health "me" 222) ;; Right
(defn- enforce-max-health
  [{:keys [name health]}]
  (fn [character-data]
    (or (<= (:health character-data) health)
        (throw (IllegalStateException. (str name " is already at max health!"))))))
(enforce-max-health {:name "me" :health 222}) ;; Right
(defn- enforce-max-health
  [& {:keys [name health]}]
  (fn [character-data]
    (or (<= (:health character-data) health)
        (throw (IllegalStateException. (str name " is already at max health!"))))))
(enforce-max-health :name "me" :health 222) ;; Right
(merge {:val 9999} {:val 100})
(merge {:val 9999} {:val 1000})
;;08/12/2017
;;o.a(1,2).b(3,4).c(5,6)
(.c (.b (.a o 1 2) 3 4) 5 6)
;;lst.filter(odd()).map(square()).reduce(add())
(->> lst
    (filter odd?)
    (map #(* % %))
    (reduce +))
(transduce +
           (comp (filter odd?) (map #(* % %)))
           lst)
(reduce + (map #(* % %) (filter odd? lst)))
