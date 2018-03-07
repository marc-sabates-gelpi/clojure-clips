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
;;03/03/2018
;;Should throw an error (more than 1 variadic)
(fn ewe
  ([] '())
  ([a & args] (into '(a) args))
  ([a b & args] (into '(a b) args)))
;;Condition-map is the first expr iff there is 1 or more expr as body
(fn ewe [a b]
  {:pre [(some? a) (some? b)]
   :post [(some? %)]}) ;; => CompilerException java.lang.RuntimeException: Unable to resolve symbol: % in this context
(defn ewe [a b]
  {:pre [(some? a) (some? b)]
   :post [(some? %)]}
  nil)
(ewe nil 1)
(ewe 1 2)
(defn ewe [a b]
  {:pre [(some? a) (some? b)]
   :post [(some? %)]}
  (str a b))
(ewe 1 2)
;;Condition-map keys are optional?
(defn ewe [a]
  {[(some? a)]
   [(some? %)]}
  a)
(ewe nil) ;; Ah it meant either entryset is optional!
(defn ewe [a]
  {:pre [(some? a)]}
  a)
(ewe nil)
;;Can the condition map be unordered?
(defn ewe [a]
  {:post [(some? %)]
   :pre [(some? a)]}
  a)
(ewe nil)
;;Condition-map can be in the fn metadata arglist?
(meta #'ewe)
(defn ^{:arglist {:pre [(some? a)] :post [(some? %)]}} ewe [a] a) => ?!?!
;;fn defines a recursion point at the top & loop does as well
(defn ewe [a]
  (loop [aa a bb a]
    (cond
      (= 0 aa) (recur (dec aa))
      (= 0 (mod aa 2)) (recur (dec aa) (dec bb))
      :else :end))) ;; => Mismatched argument count to recur, expected: 2 args, got: 1
;; the recursion points must be mutualy exclusive
(defn ewe [a]
  (if (= 0 a)
    :end
    (recur (dec a))))
(ewe 4)
(defn ewe [a]
  (loop [aa a bb (rand-int 10)]
    (if (or (= 0 aa) (= 0 bb))
      :end
      (recur (dec aa) (dec bb)))))
(ewe 4)
;;last expr in a catch expr is the new return value
(defn ewe []
  (try
    (throw (Exception. "My exception"))
    (catch Exception e (do
                         (prn "Exception!")
                         ##Inf))))
(ewe)
;;Does finally expr become the new return value or it is only executed for side effects?
(defn ewe []
  (try
    (throw (Exception. "My exception"))
    (catch Exception e (do
                         (prn "Exception!")
                         ##Inf))
    (finally ##NaN)))
(ewe) ;; The answer is NO, finally is not the return value!
;;Destructuring treats vector literal abstractedly?
(defn ewe [[a b]]
  (list a b))
(ewe '(1 2))
(ewe [1 2])
(ewe "12")
(ewe {1 1 2 2}) ;; => UnsupportedOperationException nth not supported on this typ
(ewe #{1 2}) ;; => idem
;;...and map literal abstractedly?
(defn ewe [{:keys [one two]}]
  (list one two))
(ewe [:one :two]) ;; => (nil nil)
(ewe {:one 1 :two 2})
(ewe #{:one :two})
(defn ewe [{zero 0 one 1}]
  (list zero one))
(ewe [4 5])
(ewe {0 6 1 9})
;;04/03/2018
;; from emccue @ clojurians
(defn convert-to-class [^Class class clojure-obj]
  "Converts the clojure object to the given POJO class using jackson"
  (let [^String json-str (to-json-str clojure-obj)
        mapper (doto (ObjectMapper.))]
    (.readValue mapper json-str class)))
;;refactor
(defn convert-to-class [^Class class clojure-obj]
  "Converts the clojure object to the given POJO class using jackson"
  (let [^String json-str (to-json-str clojure-obj)]
    (doto (ObjectMapper.) (.readValue json-str class))))

;;06/03/2018
;;--- Day 1: No Time for a Taxicab --- Part 1

(def ^:const N [0 1])
(def ^:const E [1 0])
(def ^:const S [0 -1])
(def ^:const W [-1 0])
(def ^:const HQ-RULES #:hq-rule{{:orient N :instr :R} E
                                {:orient S :instr :L} E
                                {:orient N :instr :L} W
                                {:orient S :instr :R} W
                                {:orient E :instr :R} S
                                {:orient W :instr :L} S
                                {:orient E :instr :L} N
                                {:orient W :instr :R} N})
(defn mult [[x y] n]
  [(* x n) (* y n)])
(-> "resources/aoc2016/hq-instructions"
    slurp
    (clojure.string/replace #"\s" "")
    (as-> s (re-seq #"([LR])([0-9]+)" s))
    (as-> instrs
        (map (fn [[_ dir steps]]
               (hash-map :instr (keyword dir) :steps (read-string steps)))
             instrs)
      (loop [state {:dir N :intermediate-pos [[0 0]]} updated-instrs instrs]
        (if (empty? updated-instrs)
          state
          (recur (let [current-instr (first updated-instrs)
                       current-dir (:dir state)
                       next-dir (get HQ-RULES {:orient current-dir :instr (:instr current-instr)})]
                   (-> state
                       (assoc :dir next-dir)
                       (update :intermediate-pos conj (mult next-dir (:steps current-instr)))))
                 (rest updated-instrs))))
      (reduce (fn [[res-x res-y] [elem-x elem-y]]
                [(+ res-x elem-x)
                 (+ res-y elem-y)]) (:intermediate-pos instrs))
      (+ (math/abs (get instrs 0)) (math/abs (get instrs 1)))))
;; -- Part 2
(-> "resources/aoc2016/hq-instructions"
    slurp
    (clojure.string/replace #"\s" "")
    (as-> s (re-seq #"([LR])([0-9]+)" s))
    (as-> instrs
        (map (fn [[_ dir steps]]
               (hash-map :instr (keyword dir) :steps (read-string steps)))
             instrs)
      (loop [state {:dir N :intermediate-pos [[0 0]]} updated-instrs instrs]
        (if (empty? updated-instrs)
          (:intermediate-pos state)
          (recur (let [current-instr (first updated-instrs)
                       current-dir (:dir state)
                       next-dir (get HQ-RULES {:orient current-dir :instr (:instr current-instr)})]
                   (-> state
                       (assoc :dir next-dir)
                       (update :intermediate-pos conj (mult next-dir (:steps current-instr)))))
                 (rest updated-instrs))))
      (reduce (fn [coll [x y]]
                (let [[last-x last-y] (last coll)]
                  (conj coll [((fnil + 0) last-x x) ((fnil + 0) last-y y)])))
              []
              instrs)
      ;; return first repeated
      ;; (let [freq (frequencies instrs)]
      ;;   (keep #(> 1 (get freq %)) instrs))
      ;; (+ (math/abs (get instrs 0)) (math/abs (get instrs 1)))
      ))
(let [coll [[1 1] [0 1]]] (filter #(some #{%} coll) coll))
(some #{1} '(0 1))
(ffirst (frequencies [[1 1] [0 1]]))
;; refactor is the next task
