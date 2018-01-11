(ns clips.core
  (:require [clojure.core.async :as async]))
;;25/10/2017
(count (filter sorted? (take 1000 (repeatedly #(group-by identity (range 50))))))
(take 12 (repeatedly #(group-by identity (range 50))))
(sorted? {1 [] 3 []})
(sorted? (keys {1 [] 2 []}))
(#(reduce
    (fn [ret x]
      (let [k (% x)]
        (assoc ret k (conj (get ret k []) x))))
    (sorted-map) %2)
 identity
 (range 50))
(#(map key ((fn [c] (reduce
                      (fn [ret x]
                        (assoc ret x (conj (get ret x []) x)))
                      (sorted-map) c)) %))
 [1 2 1 3 1 2 4])
(#(map key ((fn [c] (reduce
                      (fn [ret x]
                        (assoc ret x (conj (get ret x []) x)))
                      (sorted-map) c)) %))
 (range 50))
(#(map key ((fn [c] (reduce
                      (fn [ret x]
                        (assoc ret x (conj (get ret x []) x)))
                      (sorted-map) c)) %))
 '([2 4] [1 2] [1 3] [1 3]))
(assoc (sorted-map-by (comparator (fn [a b] (< (val a) (val b))))) 1 101 3 102 2 103) ;; comparator only get the keys
(#(map key (sort-by
            (fn [a]
              (val a))
            (reduce
             (fn [ret x]
               (if (contains? ret x)
                 ret
                 (assoc ret x (count ret))))
             {}
             %)))
 '([2 4] [1 2] [1 3] [1 3]))
(sort-by (fn [a] (:index (key a))) (clojure.set/index (reduce
                                           (fn [ret x]
                                             (if (some (fn [a] (= (:k a) x)) ret)
                                               ret
                                               (conj ret {:k x :index (count ret)})))
                                           #{}
                                           '([2 4] [1 2] [1 3] [1 3])) [:index]))
(reduce
 (fn [ret x]
   (if (some (fn [a] (= (:k a) x)) ret)
     ret
     (conj ret {:k x :index (count ret)})))
 #{}
 '([2 4] [1 2] [1 3] [1 3]))
(((fn [& fs] (fn [& args] (map (fn [f] (apply f args)) fs))) + max min) 2 3 5 1 6 4)
(= (take 5 ((fn ([f coll] (reduce (fn [xs x] (conj xs (f x (peek xs)))) [] coll)) ([f val coll] (do))) + '(0 1 2 3 4))) [0 1 3 6 10])
(let [xs [0] x 1] (conj xs (+ x (peek xs))))
;;26/10/2017
((fn [f coll]
    (reduce
     (fn [xs x]
       (let [prev (peek xs)]
         (conj xs (if prev (f prev x) (f x)))))
     []
     coll))
 +
 (range 5))
(apply + 1 (peek [0]))
(apply + 1 nil [])
(+ 1 nil)
(into 1 nil)
(+ 1)
((fn [f val coll]
    (reduce
     (fn [xs x]
       (let [prev (peek xs)]
         (conj xs (if prev (f prev x) (f x)))))
     (vector val)
     coll))
 conj [1] [2 3 4])
(vector)
(take 4
      ((fn r
         ([f coll]
          (let [c (f (first coll))]
            (lazy-seq (cons c (r f c (drop 1 coll))))))
         ([f val coll]
          (let [c (f val (first coll))]
            (lazy-seq (cons c (r f c (drop 1 coll)))))))
       +
       (range 5)))
;; 29/10/2017
(take 4 ((fn r
           ([f coll]
            (let [c (f (first coll))]
              (lazy-seq (cons c (r f c (drop 1 coll))))))
           ([f val coll]
            (let [c (f val (first coll))]
              (lazy-seq (cons val (r f c (drop 1 coll)))))))
         +
         (range 5)))
(defn r [f val coll]
  (let [c (f val (first coll))]
    (lazy-seq (cons c (r f c (drop 1 coll))))))
(realized? (take 3 (r + 0 (range 10))))
(realized? (take 3 (
                    (fn r [f val coll]
                      (let [c (f val (first coll))]
                        (lazy-seq (cons val (r f c (drop 1 coll))))))
                    +
                    0
                    (range 4))))
(take 5 ((fn r
           ([f coll]
            (let [fst (first coll)]
              (lazy-seq (cons fst (r f fst (drop 1 coll))))))
           ([f val coll]
            (let [curr (f val (first coll))]
              (lazy-seq (cons curr (r f curr (drop 1 coll)))))))
         +
         (range)))
(reduce #(let [res (+ % %2)] (println res) res) (range 5))
(defn B
  [f prev coll]
  (when-let [fst (first coll)]
    (let [curr (f prev fst)]
      (lazy-seq (cons curr (B f curr (drop 1 coll)))))))
(defn A
  ([f coll]
   (let [fst (first coll)]
     (lazy-seq (cons fst (B f fst (drop 1 coll))))))
  ([f val coll]
   (lazy-seq (cons val (B f val coll)))))
(take 5 (A + (range)))
(A conj [1] [2 3 4])
(defn A
  [f & [val coll]]
  (let [B (fn B
            [f prev coll]
            (when-let [fst (first coll)]
              (let [curr (f prev fst)]
                (lazy-seq (cons curr (B f curr (drop 1 coll)))))))]
    (if-not coll
      (let [coll val fst (first coll)]
        (lazy-seq (cons fst (B f fst (drop 1 coll)))))
      (lazy-seq (cons val (B f val coll))))))
;;30/10/2017
(take 5 (reductions + (range)))
(reductions + '(0 1 2 3 4))
(let [B (fn B
          [f prev coll]
          (when-let [fst (first coll)]
              (let [curr (f prev fst)]
                (lazy-seq (cons curr (B f curr (drop 1 coll)))))))]
  (fn
    ([f coll]
       (when-let [fst (first coll)]
         (lazy-seq (cons fst (B f fst (drop 1 coll))))))
    ([f val coll]
     (lazy-seq (cons val (B f val coll))))))
(first [1 2])
(lazy-seq (cons nil nil))
'(nil)
(reduced :big)
(reductions + (range 5))
(fn r
  ([f coll]
   (lazy-seq
    (if-let [s (seq coll)]
      (r f (first s) (rest s))
      (list (f)))))
  ([f init coll]
   (lazy-seq
    (cons init 
          (when-let [s (seq coll)]
            (r f (f (first s)) (rest s)))))))
(= (take 5 ((fn rr
  ([f coll]
   (lazy-seq
    (if-let [s (seq coll)]
      (rr f (first s) (rest s))
      (list (f)))))
  ([f init coll]
   (lazy-seq
    (cons init 
          (when-let [s (seq coll)]
            (rr f (f init (first s)) (rest s))))))) + (range))) [0 1 3 6 10])
((fn rr
  ([f coll]
   (lazy-seq
    (if-let [s (seq coll)]
      (rr f (first s) (rest s))
      (list (f)))))
  ([f init coll]
   (lazy-seq
    (cons init 
          (when-let [s (seq coll)]
            (rr f (f init (first s)) (rest s))))))) + nil)
(seq '())
(seq nil)
(seq '(nil))
(cons 1 nil)
(#(array-map (map (fn [a b] {a b}) % %2))
 [:a :b :c] [1 2 3])
(#(reduce (fn [xs [k v]] (assoc xs k v)) {} (map (fn [a b] [a b]) % %2))
 [:a :b :c] [1 2 3])
(#(reduce (fn [xs [k v]] (assoc xs k v)) {} (map (fn [a b] [a b]) % %2))
 [:a :b :c] [1 2])
(fn r [f init] (lazy-seq (cons init (r f (f init)))))
((fn [f coll] (reduce (fn [xs x] (let [k (f x)] (assoc xs k (conj (get xs k []) x)))) {} coll))
 #(> % 5) [1 3 6 8])
;;2/11/2017
(#(list (time
       (
        (fn m [x y]
          (if (= y 0)
            0
            (let [r (bit-shift-left (m x (bit-shift-right y 1)) 1)]
              (if (odd? y)
                (+ x r)
                r))))
        % %2))
      (time ((fn m2 [x y]
          (if (= y 0)
            0
            (let [r (* 2 (m2 x (int (/ y 2))))]
              (if (odd? y)
                (+ x r)
                r)))) % %2)))
 7877 7879)
(time (* 10210101010101010N 20202020202020202N))
(int (/ 2 3))
(not 0)
(false? 0)
(byte 2r1010)
(type 2r1010)
(bit-xor 2r000 2r111)
(bit-shift-right 2r011 1)
(bit-shift-right 3 1)
(bit-shift-left 2r001 1)
(bit-shift-left 1 1)
(= (mod (+ 11 77) 6) (mod (+ (mod 11 6) (mod 77 6)) 6))
(= (mod (* 11 77) 6) (mod (* (mod 11 6) (mod 77 6)) 6))
;;5/11/2017
(doto (String. "new") (.toString))
(doto "no new" (.toString))
;;22/11/17
(< "a" "b")
(compare "a" "b")
(< 1 2)
(#(case % "a" true ("b" "c") false) "b")
(seq '())
(defmacro unless
  [condition consequent]
  `(when-not ~condition ~consequent))
(unless (< 2 1) (println "ok"))
(defmacro prnx
  []
  `(println ~'x))
(let [x 1] (prnx))
(macroexpand-1 '(prnx))
(defmacro prnx
  []
  `(println ~x))
(def x 3)
(defmacro prnx
  []
  `(println ~clips.core/x))
(let [x 2] (prnx))
