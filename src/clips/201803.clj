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
(-> ;; "resources/aoc2016/hq-instructions"
 ;; slurp
 "R8, R4, R4, R8"
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
      (loop [found nil [current & remaining] instrs]
        (if (or (some? found) (nil? current))
          found
          (recur
           (if (some #{current} remaining)
             current
             nil)
           remaining)))
      ;; (+ (math/abs (get instrs 0)) (math/abs (get instrs 1)))
      ))
(let [coll [[1 1] [0 1]]] (filter #(some #{%} coll) coll))
(some #{1} '(0 1))
(ffirst (frequencies [[1 1] [0 1]]))
;; -- Part 2 requires something completely different, I'll be refactoring Part 1 instead
(defn intersection-from-last []
  (fn [xf]
    (let [last-dir (volatile! N)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result {:keys [instr steps]}]
         (let [prior-dir @last-dir
               next-dir (get HQ-RULES {:orient prior-dir :instr instr})]
            (vreset! last-dir next-dir)
            (xf result (mult next-dir steps))))))))

(-> "resources/aoc2016/hq-instructions"
    slurp
    (clojure.string/replace #"\s" "")
    (as-> input
      (re-seq #"([LR])([0-9]+)" input)
      (transduce (comp
                  (map (fn [[_ dir steps]]
                         (hash-map
                          :instr (keyword dir)
                          :steps (read-string steps))))
                  (intersection-from-last))
                 (fn
                   ([] [0 0]) ;; init transduce
                   ([[x y]] (+ (math/abs x) (math/abs y))) ;; last transduce step
                   ([[acc-x acc-y] [elem-x elem-y]] ;; regular transduce step
                    [(+ acc-x elem-x)
                     (+ acc-y elem-y)]))
                 input)))
;;08/03/2018
;; AoC 2016 - Day 1 - Part 2
(defn vertex-from-last []
  (fn [xf]
    (let [last-pos (volatile! [0 0])]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result [x y :as diff]]
         (let [[prior-x prior-y] @last-pos
               next-pos [(+ x prior-x) (+ y prior-y)]]
           (vreset! last-pos next-pos)
           (xf result next-pos)))))))
(defn my-range [a b]
  (let [step-f (if (> a b) dec inc)]
    (take (math/abs (- a b)) (drop 1 (iterate step-f a)))))
(defn fill-gaps-from-last []
  (fn [xf]
    (let [last-pos (volatile! [0 0])
          started (volatile! false)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result [x y :as pos]]
         (let [[prior-x prior-y :as prior-pos] @last-pos
               new-positions (if (= x prior-x)
                               (for [yy (my-range prior-y y)]
                                 [x yy])
                               (for [xx (my-range prior-x x)]
                                 [xx y]))]
           (vreset! last-pos pos)
           (if @started
             (xf result new-positions)
             (do
               (vreset! started true)
               (xf [[0 0]] new-positions)))))))))
(-> "resources/aoc2016/hq-instructions"
    slurp
    ;; "R8, R4, R4, R8"
    (clojure.string/replace #"\s" "")
    (as-> input
      (re-seq #"([LR])([0-9]+)" input)
      (transduce (comp
                  (map (fn [[_ dir steps]]
                         (hash-map
                          :instr (keyword dir)
                          :steps (read-string steps))))
                  (intersection-from-last)
                  (vertex-from-last)
                  (fill-gaps-from-last))
                 (fn
                   ([] [])
                   ([result] (identity result))
                   ([result input]
                    (into result input)))
                 input)
      (reduce (fn [{:keys [found coll] :as state} item]
                (cond
                  (some? found) state
                  (some #{item} coll) (assoc state :found item)
                  :else (update state :coll conj item)))
              {:found nil :coll []}
              input)
      (:found input)
      (some-> input
              ((fn [[x y]] (+ (math/abs x) (math/abs y))))))
    clojure.pprint/pprint)

(sequence (interpose \*) '(\a \b \c \d))
(into [] '(1 2 3))
(into [] '())
(range 1 3 1)
(range -1 2 1)
(range 1 3 -1)
;; Day 1 has been an exercise to explore transducers closer,
;; but the solution is overcomplicated!

;;10/03/2018
get-in
get
filter
(def json-obj {:type :root :result [{:type :a :val :val-a} {:type :b :val [{:type :b1 :val :val-b1} {:type :b2 :val :val-b2}]} {:type :c :val :valc}]})
(defn type? [type node] (= type (:type node)))
(-> json-obj
    :result
    (as-> res (filter (partial type? :b) res))
    prn)
(defn get-by-type [type node] (when (= type (:type node)) node))
(-> json-obj
    :result
    (as-> res (some (partial get-by-type :b) res))
    prn)
rest
next
(clojure.walk/walk identity #(do (prn (format "[walk] element: %s" %)) %) json-obj)
(clojure.walk/prewalk-demo json-obj)
(clojure.walk/prewalk #(do (prn (format "[prewalk] element: %s" %)) %) json-obj)
(clojure.walk/postwalk-demo json-obj)
(clojure.walk/postwalk #(do (prn (format "[postwalk] element: %s" %)) %) json-obj)
;;11/03/2018
;; How worthwhile would be to have a reduce that can run multiple reduce
;; at the same time vs going through a collection multiple times?
(defmacro inthread-time
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  [prefix expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str (format "[%s] " ~prefix) "Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))
(def ^:conts COLLSIZE 1000000)
(future
  (inthread-time "SEPARATE"
   (let [bigcoll (range COLLSIZE)]
     (transduce (map inc) + 0N bigcoll)
     (transduce (map inc) * 1N bigcoll)
     nil)))
(defn add-and-mul-reducer
  ([] [0N 1N])
  ([result] result)
  ([[add-total mul-total] el] [(+ add-total el) (* mul-total el)]))
(future
  (inthread-time "COMPOUND"
   (do
     (transduce (map inc) add-and-mul-reducer (range COLLSIZE))
     nil)))
(defmacro m-transduce
  "Transduce with support for multiple reduce functions"
  [xform fs inits coll]
  `(transduce ~xform
              (completing
                      (fn
                        ([res1#] res1#)
                        ([resn# el#] (map
                                      (fn [ff# res#]
                                        (ff# res# el#))
                                      ~fs
                                      resn#))))
              ~inits
              ~coll))
(clojure.pprint/pprint (macroexpand-1 '(m-transduce (map inc) (list + *) (list 0N 1N) (range 10))))
;; Well It is not finished but I just got the results for a million
;; elements:
;; "[SEPARATE] Elapsed time: 2740235.579406 msecs"
;; "[COMPOUND] Elapsed time: 2745969.020781 msecs"
;; So it is NOT worthwhile
(= (m-transduce (map inc) (list + *) (list 0N 1N) (range 10))
   (transduce (map inc) add-and-mul-reducer [0N 1N] (range 10)))

;; --- Day 2: Bathroom Security --- Part 1
(drop 1 (map #(mod % 3) (range 10)))
(defmulti padlock-move (fn [_ dir] dir))
(defmethod padlock-move :u [{:keys [pos] :as state} _]
  (let [next (- pos 3)]
    (assoc state :pos (if (pos? next) next pos))))
(defmethod padlock-move :d [{:keys [pos] :as state} _]
  (let [next (+ pos 3)]
    (assoc state :pos (if (>= 9 next) next pos))))
(defmethod padlock-move :l [{:keys [pos] :as state} _]
  (update state :pos (if (= 1 (mod pos 3)) identity dec)))
(defmethod padlock-move :r [{:keys [pos] :as state} _]
  (update state :pos (if (= 0 (mod pos 3)) identity inc)))
(-> {:pos 5}
    (padlock-move :u)
    (padlock-move :l)
    (padlock-move :l)
    (as-> p (do (prn p) p))
    (padlock-move :r)
    (padlock-move :r)
    (padlock-move :d)
    (padlock-move :d)
    (padlock-move :d)
    (as-> p (do (prn p) p))
    (padlock-move :l)
    (padlock-move :u)
    (padlock-move :r)
    (padlock-move :d)
    (padlock-move :l)
    (as-> p (do (prn p) p))
    (padlock-move :u)
    (padlock-move :u)
    (padlock-move :u)
    (padlock-move :u)
    (padlock-move :d)
    (as-> p (do (prn p) p)))
(defn find-padlock-comb [state instrs]
  (as-> (reduce padlock-move state instrs) state
    (update state :buttons conj (:pos state))))
(-> "resources/aoc2016/padlock"
    slurp
    (clojure.string/split-lines)
    (as-> lines (transduce (comp
                            (map sequence)
                            (map #(map
                                   (fn [x] (-> x clojure.string/lower-case keyword))
                                   %)))
                           (completing find-padlock-comb)
                           {:pos 5 :buttons []}
                           lines))
    :buttons
    prn)
;; Aside while reading
;; https://metail.com/technology/advent-of-code-or-how-i-learned-to-love-refactoring/
;; (AoC 2015 Day 1 Part 1)
(-> "resources/aoc2015/notquitelisp"
    slurp
    frequencies
    (#(- (get % \( 0) (get % \) 0))))
;; (AoC 2015 Day 1 Part 2)
(def safe+ (fnil + 0))
(-> "resources/aoc2015/notquitelisp"
    slurp
 ;; "()())"
    (clojure.string/replace #"\(" "1")
    (clojure.string/replace #"\)" "-1")
    (#(re-seq #"-1|1" %))
    (#(map read-string %))
    (#(reduce (fn [{:keys [ix sum] :as state} curr]
                (let [next (safe+ sum curr)]
                  (cond
                    (reduced? state) state
                    (neg? next) (-> state
                                    (assoc :basement ix)
                                    reduced)
                    :else (-> state
                              (assoc :sum next)
                              (update :ix inc)))))
              {:ix 1}
              %))
    :basement)
(first "abc")
(seq "abc")
(sequence "abc")
slurp
(-> "resources/aoc2015/notquitelisp"
    slurp
    (clojure.string/replace #"\(" "1")
    (clojure.string/replace #"\)" "-1")
    (#(re-seq #"-1|1" %))
    (#(transduce
       (map-indexed (fn [ix v] [ix (read-string v)]))
       (completing (fn [res [ix v]]
                     (let [next (+ res v)]
                       (if (neg? next)
                         (reduced ix)
                         next))))
       0
       %))
    inc)
;;13/03/2018
(def ^:const income-tax '({:to 11500 :tax 0}
                          {:to 45000 :tax 0.2}
                          {:to 150000 :tax 0.4}
                          {:to ##Inf :tax 0.45}))
(def ^:const nic '({:to 157 :tax 0}
                   {:to 866 :tax 0.12}
                   {:to ##Inf :tax 0.02}))
;; loop version (manual collection handling)
(defn- add-bottom-bounds [coll]
  (loop [prev-bottom 0 coll (seq (sort-by :to coll)) res []]
    (if coll
      (let [{:keys [to] :as curr} (first coll)]
        (recur (+ 0.01 to) (next coll) (conj res (assoc curr :from prev-bottom))))
      res)))
;; map version (automatic collection handling) but state on a volatile
(defn- make-boundary-mapper []
  (let [prev-bottom (volatile! 0)]
    (fn [{:keys [to] :as el}]
      (let [prior @prev-bottom]
        (vreset! prev-bottom (+ 0.01 to))
        (assoc el :from prior)))))
(defn- boundary-mapper [coll]
  (map (make-boundary-mapper) (sort-by :to coll)))

(add-bottom-bounds income-tax)
(boundary-mapper income-tax)
(->> (let [s 27000 weeks (/ 365 7) w (/ s weeks)]
      [(+ ;;(* (min 11500 s) 0.0)
        (* (max 0 (min (- 45000 11500) (- s 11500))) 0.2)
        (* (max 0 (min (- 150000 45000) (- s 45000))) 0.4)
        (* (max 0 (- s 150000)) 0.45))
       (* weeks
          (+ ;;(* (min 157 w) 0.0)
           (* (max 0 (min (- 866 157) (- w 157))) 0.12)
           (* (max 0 (- w 866)) 0.02)
           ))])
     (reduce +))
(defn- apply-tax [amount ranges]
  (reduce (fn [res {:keys [to from tax]}]
            (+ res
               (*
                tax
                (max 0 (min (- to from) (- amount from))))))
          0
          (boundary-mapper ranges)))
(->> (let [amount 27000 weeks-in-a-year (/ 365 7) weekly-amount (/ amount weeks-in-a-year)]
       [(apply-tax amount income-tax) 
        (* weeks-in-a-year
           (apply-tax weekly-amount nic))])
     (reduce +))
;;AoC2016 Day 2 Part 2
(def ^:const padlock [[nil nil 1 nil nil] [nil 2 3 4 nil] [5 6 7 8 9] [nil \A \B \C nil] [nil nil \D nil nil]])
(defmulti diamond-padlock-move (fn [_ dir] dir))
(defmethod diamond-padlock-move :u [{[row col] :pos :keys [pos padlock] :as state} _]
  (let [next-pos [(dec row) col]]
    (assoc state :pos (if (get-in padlock next-pos) next-pos pos))))
(defmethod diamond-padlock-move :d [{[row col] :pos :keys [pos padlock] :as state} _]
  (let [next-pos [(inc row) col]]
    (assoc state :pos (if (get-in padlock next-pos) next-pos pos))))
(defmethod diamond-padlock-move :l [{[row col] :pos :keys [pos padlock] :as state} _]
  (let [next-pos [row (dec col)]]
    (assoc state :pos (if (get-in padlock next-pos) next-pos pos))))
(defmethod diamond-padlock-move :r [{[row col] :pos :keys [pos padlock] :as state} _]
  (let [next-pos [row (inc col)]]
    (assoc state :pos (if (get-in padlock next-pos) next-pos pos))))
(defn- find-padlock-comb-with-provided-move-fn [state instrs]
  (as-> (reduce (:move-fn state) state instrs) state
    (update state :buttons conj (get-in (:padlock state) (:pos state)))))
(-> "resources/aoc2016/padlock"
    slurp
    (clojure.string/split-lines)
    (as-> lines (transduce (comp
                            (map sequence)
                            (map #(map
                                   (fn [x] (-> x clojure.string/lower-case keyword))
                                   %)))
                           (completing find-padlock-comb-with-provided-move-fn)
                           {:pos [2 0] :buttons [] :move-fn diamond-padlock-move :padlock padlock}
                           lines))
    :buttons
    prn)
;; 17/03/2018
;; AoC 2016 Day 3 Part 1
(clojure.string/split "  919  923  873" #"\t")
(clojure.string/split "  919  923  873" #"\s")
(re-seq #"[0-9]+" "  919  923  873")
(defn- valid-t? [t]
  (let [[a b c] (sort t)]
    (> (+ a b) c)))
(valid-t? '(5 20 25))
(-> "resources/aoc2016/triangles"
    slurp
 ;; "  919  923  873
 ;;  5 10 25
 ;;  6 11 30"
    clojure.string/split-lines
    (as-> lines (eduction (map #(re-seq #"[0-9]+" %))
                          (map #(map read-string %))
                          (map sort)
                          (filter valid-t?)
                          lines)) ;;Just trying out eduction I could use
                                  ;;sequence
    seq
    count)
;; Day 3 Part 2
(defn transpose-3
  "Creates a list with 3 lists resulting from gettng the first element
   of each input lists, the second of each input list and so forth.
   The input must be a collection with 3 collections with 3 nums"
  [[[top-l top-c top-r] [mid-l mid-c mid-r] [bot-l bot-c bot-r]]]
  (list
   (list top-l mid-l bot-l)
   (list top-c mid-c bot-c)
   (list top-r mid-r bot-r)))
(-> "resources/aoc2016/triangles"
    slurp
 ;; "  919  923  873
 ;;  5 10 25
 ;;  6 11 30
 ;;  6 11 26
 ;;  5 10 25
 ;;  5 10 25"
    clojure.string/split-lines
    (as-> lines (sequence (comp
                           (map #(re-seq #"[0-9]+" %))
                           (map #(map read-string %))
                           (partition-all 3)
                           (map transpose-3)
                           cat
                           (map sort)
                           (filter valid-t?))
                          lines))
    count)
(into [] (comp cat cat (map inc)) [[[1] [2]] [[3] [4]]])
;;;; Session 19/03/2018

;;; AoC 2016 Day 4 Part 1
;;; --- Day 4: Security Through Obscurity ---
(defn- parse-room
  "Parses the room data into a map."
  [coll]
  (let [name (->> coll
                 butlast
                 (apply str))
        [id checksum] ((juxt ; HACK: juxt is not really needed,
                             ;       just playing with it
                        #(re-find #"[0-9]+" %)
                        #(re-find #"[\p{Alpha}]+" %))
                       (last coll))]
    (hash-map :name name :id id :checksum checksum)))
(defn- valid-r?
  "Valid when the checksum is the 5 most frequent letters on the name.
  When same frequency then alphabetically ordered."
  [{:keys [name checksum]}]
  (= checksum (->> name
                   sort
                   frequencies
                   (sort-by val #(compare %2 %1)) ; HACK
                   (take 5)
                   (map key)
                   (apply str))))
(-> "resources/aoc2016/rooms"
    slurp
    clojure.string/split-lines
    (as-> lines (transduce (comp
                            (map #(clojure.string/split % #"-"))
                            (map parse-room)
                            (filter valid-r?)
                            (map :id)
                            (map read-string))
                           +
                           lines)))
;;;; Session 20/03/2018

;;; AoC 2016 Day 4 Part 2
(def ^:private ^:const offset-letters (dec (int \a)))
(def ^:private ^:const num-letters 26)

(defn- parse-room-2
  "Parses the room data into a map."
  [coll]
  (let [name (butlast coll)
        raw-name (apply str name) 
        [id checksum] ((juxt ; HACK: juxt is not really needed,
                             ;       just playing with it
                        #(re-find #"[0-9]+" %)
                        #(re-find #"[\p{Alpha}]+" %))
                       (last coll))]
    (hash-map :raw-name raw-name :name name :id (read-string id) :checksum checksum)))

(defn- valid-r-2?
  "Valid when the checksum is the 5 most frequent letters on the name.
  When same frequency then alphabetically ordered."
  [{:keys [raw-name checksum]}]
  (= checksum (->> raw-name
                   sort
                   frequencies
                   (sort-by val #(compare %2 %1)) ; HACK
                   (take 5)
                   (map key)
                   (apply str))))

(defn- shift-cipher-char
  "Shifts a letter by a fix amount."
  [n ch]
  (-> (int ch)
      (- offset-letters)
      (+ n)
      (mod num-letters)
      (+ offset-letters)
      char))

(defn- shift-cipher
  "Applies shift-cipher to a string."
  [n s]
  (apply str (map (partial shift-cipher-char n) s)))

(defn- decrypt-names
  "Decrypts the room names."
  [{:keys [id] :as room}]
  (update room :name (fn [coll] (->> (map (partial shift-cipher id) coll)
                                     (interpose \space)
                                     (apply str)))))

(-> "resources/aoc2016/rooms"
    slurp
    clojure.string/split-lines
    (as-> lines (sequence (comp
                            (map #(clojure.string/split % #"-"))
                            (map parse-room-2)
                            (filter valid-r-2?)
                            (map decrypt-names)
                            (filter #(re-find #"north" (:name %))))
                          lines)))

;;;; Session 23/03/2018

;;; AoC2016 Day 5 Part 1

(def ^:const ^:private passwd-len 8)
(def ^:const ^:private hash-difficulty 5)

;;; C-c RET h d [digest "1.4.6"]
(require '[digest :as digest])

(defn- seeded-indexed-md5-gen
  "An infinite generator of md5 hashes.
  The hash is got from the `seed` with an `ix`."
  ([seed] (seeded-indexed-md5-gen seed 0))
  ([seed ix]
   (lazy-seq (cons (digest/md5 (str seed ix))
                   (seeded-indexed-md5-gen seed (inc ix))))))

(defn- make-n-difficult?
  "Makes a `n` difficulty predicate.
  The predicate returns the true if `hash` begins with `n` zeros,
  and false otherwise."
  [n]
  (let [x (min 32 n)]
    (fn [hash]
      (= "0" (apply str (dedupe (take x hash)))))))

(defn- difficulty-md5-filter
  "Filters a `coll` of md5 with a certain difficulty.
  The difficulty is the `n` of zeros in the beggining of the hash."
  [n coll]
  (let [coll (drop-while (complement (make-n-difficult? n)) coll)]
    (lazy-seq (cons (first coll)
                    (difficulty-md5-filter n (rest coll))))))

(ns-unmap *ns* 'make-seeded-md5-gen)

(->> (sequence (comp
                (take passwd-len)
                (map #(get % hash-difficulty)))
               (difficulty-md5-filter
                hash-difficulty
                (seeded-indexed-md5-gen "ffykfhsq")))
    (apply str))

;;; Alternative impl with the difficulty filter in the transduce proc
(->> (sequence (comp
                (filter (make-n-difficult? hash-difficulty))
                (take passwd-len)
                (map #(get % hash-difficulty)))
               (seeded-indexed-md5-gen "ffykfhsq"))
    (apply str))

;;; 24/03/2018
;;; Structure & Interpretation test
(defn- cons [x y]
  (fn dispatch [m]
    (cond (= m 0) x ; POC, this could be using case
          (= m 1) y
          :else (throw (Exception. (str "Argument not 0 or 1 -- CONS" m))))))

(defn- car [z] (z 0))

(defn- cdr [z] (z 1))

(car (cons 1 2))
(cdr (cons 1 2))

;;; AoC2016 Day 5 Part 2

(defn- make-valid-pos?
  "Returns nil when ascii 0 > nth-`hash`, nth-`hash` > ascii 7,
  or nth-`hash` already exists."
  [n]
  (let [zero (int \0) seven (int \7) prevs (volatile! '())]
    (fn [hash]
      (let [x (int (get hash n))]
        (when (and
               (<= zero x seven)
               (not-any? #{x} @prevs))
          (vswap! prevs conj x))))))

(->> (sequence (comp
                (filter (make-n-difficult? hash-difficulty))
                (filter (make-valid-pos? hash-difficulty))
                (take passwd-len)
                (map #(vector (get % hash-difficulty) (get % (inc hash-difficulty)))))
               (seeded-indexed-md5-gen "ffykfhsq" #_"abc"))
     (sort-by (fn [[k _]] k))
     (map (fn [[_ v]] v))
     (apply str)
     prn)

;;; AoC 2016 Day 6 Part 1

(->> "resources/aoc2016/message"
     slurp
     #_"eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar" 
     clojure.string/split-lines
     ;; REVIEW: Is this clearer than the other reduce? (msg 25/03/2018)
     #_(reduce (fn [acc el] (mapv conj acc el)) (for [x (range 6)] []))
     (reduce #(apply mapv conj %&) (for [x (range #_6 8)] []))
     (sequence (comp
                (map frequencies)
                (map #(sort-by val %))
                (map reverse)
                (map first)
                (map key)))
     (apply str))

;;; Potential Cider debugger bug
;;; https://github.com/clojure-emacs/cider/issues/2251
(do
  ;;(println "I am commented out!")
  (comment (println "I am commented out, too.."))
  #_ (println "I am commented out, as well..")
  #break (+ 2 2))

;;; AoC 2016 Day 6 Part 2

(->> "resources/aoc2016/message"
     slurp
     #_"eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar" 
     clojure.string/split-lines
     (reduce #(apply mapv conj %&) (for [x (range #_6 8)] []))
     (sequence (comp
                (map frequencies)
                (map #(sort-by val %))
                (map first)
                (map key)))
     (apply str))

;;; AoC 2016 Day 7 Part 1

(defn- abba?
  "Logical true if abba and a <> b, logical false otherwise."
  [s]
  ;; REVIEW: (?!\1) is a negative lookahead containing group 1.
  ;;         It does not consume input, it only asserts.
  (re-find #".*(\p{Alnum}{1})(?!\1)(\p{Alnum}{1})\2\1.*" s))

(->> "resources/aoc2016/ips"
     slurp
     #_"abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
aaaa[qwer2]abba
abba[qwer3]aaaa
ioxxoj[asdfgh]zxcvbn
ahah[abba]asdf"
     clojure.string/split-lines
     (sequence (comp
                (filter abba?)
                (map #(re-seq #"\[\p{Alnum}+\]" %))
                (remove #(some abba? %))))
     count)

;;; AoC 2016 Day 7 Part 2

(defn- get-aba
  "Returns all the possible aba patterns on `s`."
  [s]
  (sequence (comp
             (map #(re-find #"(\p{Alnum}{1})(?!\1)(\p{Alnum}{1})\1" %))
             (remove nil?)
             (map first))
            (for [x (range (count s))] (subs s x))))

(defn- reverse-aba
  "Converts aba -> bab."
  [s]
  {:pre [(= 3 (count s)) (= (get s 0) (get s 2))]}
  (str (get s 1) (get s 0) (get s 1)))

(defn- aba-bab?
  "Returns logical true when an aba~[bab] or viceversa is found.
  The pattern can be surrounded by other chars and segments."
  [s]
  (let [hypernets (re-seq #"\[\p{Alnum}+\]" s)
        supernets (-> (reduce #(clojure.string/replace %1 %2 "-") s hypernets)
                      (clojure.string/split #"-"))
        super-aba (->> supernets
                       (map get-aba)
                       flatten
                       set)
        hyper-aba (->> hypernets
                       (map get-aba)
                       flatten
                       set
                       (map reverse-aba))]
    (some super-aba hyper-aba)))

(->> #_"aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[abzb]cdb"
    #_"aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb"
    "resources/aoc2016/ips"
    slurp
    clojure.string/split-lines
    (sequence (comp
               (filter aba-bab?)))
    count)
