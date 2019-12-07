(ns clips.core
  (:require [clojure.core.async :as async :refer :all]))

(defn search []
  (let [c (chan)
        ;; t (timeout 80)
        t (chan)]
    (close! t)
    (go (>! c (<! (slurp "http://clojuredocs.org"))))
    (go (>! c (<! (slurp "https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/rawdiff/?id=v4.13-rc7&id2=v4.12"))))
    (go (>! c "https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/rawdiff/?id=v4.13-rc7&id2=v4.13-rc6"))
    (go (loop [i 0
               ret []]
          (if (= i 3)
            ret
            (recur (inc i)
                   (conj ret (alt! [c t] ([v] v)))))))))
(take! (search) (fn [x] (println (str x))))
(close! (chan))

;; 14/10/2017
(#(map conj %) [1 2 3 4 5])
(#(map inc %) [1 2 3])
(#(reduce + %) [1 2 3])
(#(reduce conj '()  %) '(1 2 3))
(seq [1 2 3])
(= [1 2 3] '(1 2 3))
(= (#(reduce conj '() (seq %)) [1 2 3]) [3 2 1])

(defn flat [c]
  (if (and (coll? c) (not-empty c))
    (apply conj '() (flat (rest c)) (first c))
    (apply list c)))
(flat [1 2])
(apply conj '() '(2) 1)

;;15/10/2017
(concat '(1 [2] 3) [4 5])
(apply concat '(1 [2] 3) [4 5 6])
(mapcat seq '((1) (2) [3]))
(mapcat #(if (coll? %) % (conj '() %)) '(1 (2) [3]))
(mapcat #(if (coll? %) % (list %)) '(1 (2) [3]))
(mapcat (fn f [x] (if (coll? x) x (list x))) '(1 (2) [3]))
(list 3)
(list '(3))
(defn f [x]
  (mapcat
   #(if (coll? %)
      (f %)
      (list %))
   x))
(f '(1 (2) [(3) 4]))
(not-empty 1)
(defn f2 [x]
  (mapcat
   #(cond (and (coll? %) (not (coll? (first %)))) %
          (coll? %) (f2 %)
          :else (list %))
   x))
(= (f2 '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(defn f3 [x]
  (mapcat
   #(cond (and (coll? %) (not-any? coll? %)) %
          (coll? %) (f3 %)
          :else (list %))
   x))
(f3 '((1 2) 3 [4 [5 6]]))
(clojure.string/replace "aBcdEf.3X" #"[^A-Z]" "")
(reduce #(if (= (last %) %2) % (conj % %2)) [] '(1 2 2 2 3 3 2 4))
(reduce #((let [l (last %)] (if (= l %2) (conj (butlast %) (conj l %2)) (conj % %2)))) [] '(1 2 2 2 3 3 2 4))
(defn g [c]
  (reduce
   #(
     (let [l (last %)]
       (if (= l %2)
         (conj
          (butlast %)
          (conj l %2))
         (conj % %2))))
   []
   c))
(g '(1 2 2 2 3 3 2 4))

;;16/10/2017
(chan 1 (map inc))
(hash-map 1 1)
(reduce (fn [xs x] (conj xs x)) [] (range 10))
(reduce (fn [xs x] (conj xs x)) (range 10))
(reduce
 (fn
   ([xs x] (conj xs x))
   ([] ()))
 (range 10))
(reduce (fn ([xs x] (conj xs x)) ([] [])) [] [])
(reduce (fn [xs x] (conj xs x)) [] [])
(require '[clojure.core.reducers :as r])
(defn factorize
  "Naive factorization function; takes an integer n and returns a vector of
  factors."
  [n]
  (if (< n 2)
    []
    (loop [factors []
           n n
           p 2]
      (cond (= n 1) factors
            (= 0 (mod n p)) (recur (conj factors p) (quot n p) p)
            (>= (* p p) n) (conj factors n)
            (> p 2) (recur factors n (+ p 2))
            :else (recur factors n (+ p 1))))))
(defn conjmap
  ([xs x] (conj xs x))
  ([] {}))
(defn rfold
  "Parallel factorizer using r/fold."
  [nums]
  (r/fold conjmap (r/map #(hash-map % (factorize %)) nums)))

(sort (rfold (range 100)))
;;17/10/2017
(dotimes)
(doseq)
(some #{3} '(2))
(#(reduce
    (fn [xs x]
      (let [l (last xs)]
        (if (some #{x} l)
          (conj (vec (butlast xs)) (conj l x))
          (conj xs (list x)))))
    []
    %) [1 1 2 1 1 1 3 3])
(butlast ['(1) '(2)])
(butlast [1 2])
(vec '(1))
#(reduce
    (fn [xs x]
      (let [l (peek xs)]
        (if (some #{x} l)
          (conj (pop xs) (conj l x))
          (conj xs (list x)))))
    []
    %)
(concat '(1) '(2) 1 2)
(concat '((1)) '((2)) '(1) '(2))
(mapcat identity '((1) (2) (3) (4)))
(map #(list % %) '(1 2 3 4))
(list 1 1)
(#(mapcat (fn [x] (list x x)) %) [1 2 3])

;; 18/10/2017
(mapcat (fn [a b] (list a b)) '(1 2 3) '(\a \b))
#(butlast (mapcat (fn [a] (list a %)) %2))
#(pop (reduce (fn [xs x] (conj xs % x)) [] %2))
(#(rest (reduce (fn [xs x] (conj xs % x)) [] %2)) 0 '(1 2 3))
(conj '() 1 2)
(list* 1 2 '(3))
(conj [3] 1 2)
(pop [1 2])
(pop '(1 2))
(partition 3 4 '(1 2 3 4 5 6 7))
(mapcat identity (partition 3 4 '(1 2 3 4 5 6 7)))
(#(mapcat identity (partition %2 (inc %2) %)) '(1 2 3 4 5 6 7 8) 3)
(#(reduce-kv (fn [xs kx vx] (let [inc-kx (inc kx)] (if (or (not (= 0 (mod inc-kx %2))) (= 0 kx)) (conj xs vx) xs))) []  %) [1 2 3 4 5 6 7 8])
#(reduce-kv (fn [xs kx vx] (if-not (= 0 (mod (inc kx) %2)) (conj xs vx) xs)) []  %)
;;20/10/2017
#(apply * (range 1 (inc %)))
((fn [l n] (partition (quot (count l) n) n l)) '(1 2 3 4 5 6) 2)
((fn [l n] (partition (quot (count l) n) n l)) (range 9) 3)
(partition 3 3 (range 9))
;;22/10/2017
(require '[clojure.walk :as w])
(w/walk #(list (inc %)) concat [1 [2 3] '(2 1) 0])
(w/prewalk #(inc %) [1 [2 3] '(2 1) 0])
(partition 2 '(1 2 3 4 5 6))
(apply map list (partition 2 '(1 2 3 4 5 6)))
(mod -2 5)
(partition 3 3 (range 4) (range 4))
(take 2 (range 4))
(drop 2 (range 4))
(concat (drop 2 (range 4)) (take 2 (range 4)))
((fn [n coll] (let [shift (mod n (count coll))] (concat (drop shift coll) (take shift coll)))) 2 '(1 2 3 4 5))
(some)
(#{2 7 6} 6)
(type)
(#(partition-by type %) [1 :a 2 :b 3])
(#(split-with type %) [1 :a 2 :b 3])
(type "text")
(type :b)
(type 1)
(reduce (fn [xs x] (let [k (first x) v (last x)] (if (contains? xs k) (update-in xs k conj v) (assoc-in xs k [v])))) {} (map #(vector (str (type %)) %) [1 :a 2 :b 3]))
(str (type 1))
;;23/10/2017
(#(reduce
   (fn [last current]
     (if (< (count last) (count current))
       current
       last))
   '()
   (filter
    (fn [x]
      (>= (count x) 2))
    (reduce
     (fn [xs x]
       (let [last-list (peek xs) prevx (peek last-list)]
         (cond (empty? last-list) (vector (vector x))
               (= prevx (dec  x)) (assoc-in xs [(dec (count xs)) (count last-list)] x)
               :else (assoc xs (count xs) (vector x)))))
     []
     %)))
 [7 6 5 4])
(pop [[2] [1]])
(count [[2]])
(assoc [[2]] 0 [1])
(filter (comp #(>= % 2) count) [[1] [2 2] [3 4 5]])
(#(filter (fn [x] (= (count x) %)) (reduce-kv (fn [xs x k] (if (= (mod k %) 0) (conj xs (vector x)) (update-in xs [(dec (count xs))] conj x))) [] (vec %2)))
 2 (range 8))
(mod 8 3)
(#(vec (take (mod (count %2) %) %2)) 3 (range 8))
(map (fn [x] (assoc-in x [:val] (count (val x)))) (group-by identity [1 1 2 3 2 1 1]))
(group-by identity [1 1 2 3 2 1 1])
(map (fn [x] (let [[kkey value] x] {kkey (count value)})) (group-by identity [1 1 2 3 2 1 1]))
(concat '({1 2} {2 3}))
(#(let [freq (group-by identity %)] (zipmap (keys freq) (map (fn [x] (count x)) (vals freq)))) [1 1 2 3 2 1 1])
;;24/10/2017
(((fn [& fs] (reduce (fn [xs x] (fn [& args] (x (apply xs args)))) (reverse fs)))
  rest reverse) [1 2 3 4])
(def f #(sort (map key (group-by identity %))))
(#(sort (map key (group-by identity %))) [1 2 1 3 1 3 4])
(= (#(sort (map key (group-by identity %))) (range 50)) (range 50))
(= (f [1 2 1 3 1 2 4]) [1 2 3 4])
(group-by identity (range 9))
(group-by identity '(0 1 2 3 4 5 6 7 8))
(range 9)
(= (f '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
(def g #(reduce (fn [xs x] (if (= (last xs) x) xs (conj xs x))) [] %))
(= (g (range 50)) (range 50))
(= (g [1 2 1 3 1 2 4]) [1 2 3 4])
(def group-by-original group-by)
(defn group-by 
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  {:added "1.2"
   :static true}
  [f coll]  
  (persistent!
   (reduce
    (fn [ret x]
      (let [k (f x)]
        (assoc! ret k (conj (get ret k []) x))))
    (transient {}) coll)))
(group-by identity (range 9))
(ns-unmap 'user 'group-by)
(def group-by group-by-original)
(group-by identity (range 9))
(ns clips.core
  (:require [clojure.core.async :as async :refer :all]))
;;25/10/2017
(count (filter sorted? (take 1000 (repeatedly #(group-by identity (range 50))))))
(take 12 (repeatedly #(group-by identity (range 50))))
(#(reduce
    (fn [ret x]
      (let [k (f x)]
        (assoc! ret k (conj (get ret k []) x))))
    {} %)
(range 50))
