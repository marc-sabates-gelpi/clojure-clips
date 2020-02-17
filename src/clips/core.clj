(ns clips.core
  (:require #_[clojure.core.async :as async]
    [clj-memory-meter.core :as mm]
    [clojure.edn :as edn]
    [clojure.repl]
    [clojure.string :as string]
    [spyscope.core]))

;;;; Session 25/04/2019

(use '[clojure.repl])
(doc apropos)

;; array map
(map (fn [el] el) {:a 1 :b 2})                              ;; => ([:a 1] [:b 2])
(map identity {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8})
;; => ([:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6] [:g 7] [:h 8])
(type {:a 1 :b 2})
;; => clojure.lang.PersistentArrayMap

;; hash map
(map identity {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9}) ;; => ([:e 5] [:g 7] [:c 3] [:h 8] [:b 2] [:d 4] [:f 6] [:i 9] [:a 1])
(type {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9})
;; => clojure.lang.PersistentHashMap

;;destructure with namespaced keys
(let [{:my-ns/keys [a b] :or {a 99 b 98}} #:my-ns{:a 1}] [a b]) ;; => [1 98]


;;;; Session 02/05/2019

;; case with a list of test expr
(defn test-case
  [n]
  (case (* n n)
    (1 4 9 16 25) :1-5
    (36 49 64 81 100) :6-10
    :>10))

(for [n (range 1 12)]
  (test-case n))
;; => (:1-5 :1-5 :1-5 :1-5 :1-5 :6-10 :6-10 :6-10 :6-10 :6-10 :>10)

(macroexpand `#(* %1 %9))                                   ;; => (fn* [p1__13404__13413__auto__ p2__13406__13414__auto__ p3__13407__13415__auto__ p4__13408__13416__auto__ p5__13409__13417__auto__ p6__13410__13418__auto__ p7__13411__13419__auto__ p8__13412__13420__auto__ p9__13405__13421__auto__] (clojure.core/* p1__13404__13413__auto__ p9__13405__13421__auto__))

(macroexpand `#((+ 1 2)))
;; => (fn* [] ((clojure.core/+ 1 2)))

(macroexpand `#(do (+ 1 2) (* 1 2)))                        ;; => (fn* [] (do (clojure.core/+ 1 2) (clojure.core/* 1 2)))

*compiler-options*                                          ;; => nil
*data-readers*                                              ;; => {dbg #'cider.nrepl.middleware.debug/debug-reader, break #'cider.nrepl.middleware.debug/breakpoint-reader, light #'cider.nrepl.middleware.enlighten/light-reader}
clojure.core/default-data-readers                           ;; => {uuid #'clojure.uuid/default-uuid-reader, inst #'clojure.instant/read-instant-date}

(read-string "#=(+ 1 1)")                                   ;; => 2
(binding [*read-eval* false] (read-string "#=(+ 1 1)"))
*e                                                          ;; => #error {
;; :cause "EvalReader not allowed when *read-eval* is false."
;; :via
;; [{:type java.lang.RuntimeException
;;   :message "EvalReader not allowed when *read-eval* is false."
;;   :at [clojure.lang.Util runtimeException "Util.java" 221]}]
;; :trace
;; [[clojure.lang.Util runtimeException "Util.java" 221]
;;  [clojure.lang.LispReader$EvalReader invoke "LispReader.java" 1297]
;;  [clojure.lang.LispReader$DispatchReader invoke "LispReader.java" 853]
;;  [clojure.lang.LispReader read "LispReader.java" 285]
;;  [clojure.lang.LispReader read "LispReader.java" 216]
;;  [clojure.lang.LispReader read "LispReader.java" 205]
;;  [clojure.lang.RT readString "RT.java" 1874]
;;  [clojure.lang.RT readString "RT.java" 1869]
;;  [clojure.core$read_string invokeStatic "core.clj" 3815]
;;  [clojure.core$read_string invoke "core.clj" 3805]
;;  [clips.core$eval13448 invokeStatic "NO_SOURCE_FILE" 55]
;;  [clips.core$eval13448 invoke "NO_SOURCE_FILE" 55]
;;  [clojure.lang.Compiler eval "Compiler.java" 7176]
;;  [clojure.lang.Compiler eval "Compiler.java" 7131]
;;  [clojure.core$eval invokeStatic "core.clj" 3214]
;;  [clojure.core$eval invoke "core.clj" 3210]
;;  [clojure.main$repl$read_eval_print__9068$fn__9071 invoke "main.clj" 414]
;;  [clojure.main$repl$read_eval_print__9068 invoke "main.clj" 414]
;;  [clojure.main$repl$fn__9077 invoke "main.clj" 435]
;;  [clojure.main$repl invokeStatic "main.clj" 435]
;;  [clojure.main$repl doInvoke "main.clj" 345]
;;  [clojure.lang.RestFn applyTo "RestFn.java" 137]
;;  [clojure.core$apply invokeStatic "core.clj" 665]
;;  [clojure.core$apply invoke "core.clj" 660]
;;  [refactor_nrepl.ns.slam.hound.regrow$wrap_clojure_repl$fn__9642 doInvoke "regrow.clj" 18]
;;  [clojure.lang.RestFn invoke "RestFn.java" 1523]
;;  [nrepl.middleware.interruptible_eval$evaluate invokeStatic "interruptible_eval.clj" 79]
;;  [nrepl.middleware.interruptible_eval$evaluate invoke "interruptible_eval.clj" 55]
;;  [nrepl.middleware.interruptible_eval$interruptible_eval$fn__935$fn__939 invoke "interruptible_eval.clj" 142]
;;  [clojure.lang.AFn run "AFn.java" 22]
;;  [nrepl.middleware.session$session_exec$main_loop__1036$fn__1040 invoke "session.clj" 171]
;;  [nrepl.middleware.session$session_exec$main_loop__1036 invoke "session.clj" 170]
;;  [clojure.lang.AFn run "AFn.java" 22]
;;  [java.lang.Thread run "Thread.java" 748]]}

(macroexpand `(-> 2 inc (* 2)))                             ;; => (clojure.core/* (clojure.core/inc 2) 2)

;;;; Session 19/05/2019

(let [{::keys [a]} #:my.namespace{:a 1}] (inc a))
*e                                                          ;; => #error {
;;     :cause nil
;;     :via
;;     [{:type java.lang.NullPointerException
;;       :message nil
;;       :at [clojure.lang.Numbers ops "Numbers.java" 1068]}]
;;     :trace
;;     [[clojure.lang.Numbers ops "Numbers.java" 1068]
;;      [clojure.lang.Numbers inc "Numbers.java" 137]
;;      [clips.core$eval24439 invokeStatic "NO_SOURCE_FILE" 102]
;;      [clips.core$eval24439 invoke "NO_SOURCE_FILE" 102]
;;      [clojure.lang.Compiler eval "Compiler.java" 7176]
;;      [clojure.lang.Compiler eval "Compiler.java" 7131]
;;      [clojure.core$eval invokeStatic "core.clj" 3214]
;;      [clojure.core$eval invoke "core.clj" 3210]
;;      [clojure.main$repl$read_eval_print__9068$fn__9071 invoke "main.clj" 414]
;;      [clojure.main$repl$read_eval_print__9068 invoke "main.clj" 414]
;;      [clojure.main$repl$fn__9077 invoke "main.clj" 435]
;;      [clojure.main$repl invokeStatic "main.clj" 435]
;;      [clojure.main$repl doInvoke "main.clj" 345]
;;      [clojure.lang.RestFn applyTo "RestFn.java" 137]
;;      [clojure.core$apply invokeStatic "core.clj" 665]
;;      [clojure.core$apply invoke "core.clj" 660]
;;      [refactor_nrepl.ns.slam.hound.regrow$wrap_clojure_repl$fn__9698 doInvoke "regrow.clj" 18]
;;      [clojure.lang.RestFn invoke "RestFn.java" 1523]
;;      [nrepl.middleware.interruptible_eval$evaluate invokeStatic "interruptible_eval.clj" 79]
;;      [nrepl.middleware.interruptible_eval$evaluate invoke "interruptible_eval.clj" 55]
;;      [nrepl.middleware.interruptible_eval$interruptible_eval$fn__935$fn__939 invoke "interruptible_eval.clj" 142]
;;      [clojure.lang.AFn run "AFn.java" 22]
;;      [nrepl.middleware.session$session_exec$main_loop__1036$fn__1040 invoke "session.clj" 171]
;;      [nrepl.middleware.session$session_exec$main_loop__1036 invoke "session.clj" 170]
;;      [clojure.lang.AFn run "AFn.java" 22]
;;      [java.lang.Thread run "Thread.java" 748]]}

(let [{::keys [a]} #:clips.core{:a 1}] (inc 1))             ;; => 2

(some identity [nil 1 nil 3])                               ;; => 1

()                                                          ;; => ()
(type ())                                                   ;; => clojure.lang.PersistentList$EmptyList
;; It is not a fn call.. Weird!

'()                                                         ;; => ()
(type '())                                                  ;; => clojure.lang.PersistentList$EmptyList


;;;; Session 31/08/2019

(defmacro if-let-xf
  "Like `if-let` but it applies `xf` to the `if` reference symbol on branch 1.
  It supports `:let` plus a vector like in a `for`."
  [xf# [ref-symbol# assign-form# let?# extra-args#] & [branch1# branch2#]]
  (let [orig-let-vec# (if (#{:let} let?#)
                        (into [ref-symbol# assign-form#] extra-args#)
                        [ref-symbol# assign-form#])]
    `(let ~orig-let-vec#
       (if ~ref-symbol#
         (let [~ref-symbol# (~xf# ~ref-symbol#)]
           ~branch1#)
         ~branch2#))))

;;;; Session 14/11/2019

(let [f inc]
  (into {} (map (juxt key (comp f val))) {:a 1 :b 2}))

(defn sexy-apply-f
  [f m]
  (into {} (map (juxt key (comp f val))) m))

;;;; Session 16/11/2019
(->> (for [x (range 8)]
       [x x])
     (into {})
     type)
;=> clojure.lang.PersistentArrayMap
(->> (for [x (range 9)]
       [x x])
     (into {})
     type)
;=> clojure.lang.PersistentHashMap

;;;; Session 03/02/2020
;; Wes Hall  12:20
(defn seq->map
  [s key-fn val-fn]
  (reduce #(assoc %1 (key-fn %2) (val-fn %2)) {} s))

;; Proposal
(defn my-seq->map
  [s key-fn val-fn]
  (into {} (map (juxt key-fn val-fn) s)))
;; Darn!

;;;; Session 14/02/2020
;; RCFOTD - peterwestmacott
(let [{:keys [name ns arglists doc]} (->> (all-ns)
                                          (filter #(-> % str (clojure.string/starts-with? "clojure")))
                                          (mapcat ns-publics)
                                          vals
                                          (map meta)
                                          (filter :arglists)
                                          rand-nth)]
  (println (str ns "/" name "\n"
                arglists "\n"
                doc)))

;;;; Session 17/02/2020
(#{1 2 3} 3)
;=> 3
(#{1 2 3})
;Execution error (ArityException) at clips.core/eval1961 (core.clj:213).
;Wrong number of args (0) passed to: clojure.lang.PersistentHashSet
([1 2 3] 1)
;=> 2
([1 2 3])
;Execution error (ArityException) at clips.core/eval1969 (core.clj:215).
;Wrong number of args (0) passed to: clojure.lang.PersistentVector
('(1 2 3) 1)
;Execution error (ClassCastException) at clips.core/eval1973 (core.clj:222).
;class clojure.lang.PersistentList cannot be cast to class clojure.lang.IFn (clojure.lang.PersistentList and clojure.lang.IFn are in unnamed module of loader 'app')
('(1 2 3))
;Execution error (ClassCastException) at clips.core/eval1977 (core.clj:223).
;class clojure.lang.PersistentList cannot be cast to class clojure.lang.IFn (clojure.lang.PersistentList and clojure.lang.IFn are in unnamed module of loader 'app')
(defn my-rand [seed]
  (lazy-seq (cons (Math/abs (Math/sin seed)) (my-rand (inc seed)))))
;=> #'clips.core/my-rand
(def my-rand-coll (my-rand 1))
;=> #'clips.core/my-rand-coll
(my-rand-coll 2)
;Execution error (ClassCastException) at clips.core/eval1988 (core.clj:233).
;class clojure.lang.LazySeq cannot be cast to class clojure.lang.IFn (clojure.lang.LazySeq and clojure.lang.IFn are in unnamed module of loader 'app')
(my-rand-coll)
;Execution error (ClassCastException) at clips.core/eval1992 (core.clj:234).
;class clojure.lang.LazySeq cannot be cast to class clojure.lang.IFn (clojure.lang.LazySeq and clojure.lang.IFn are in unnamed module of loader 'app')
(defn my-rand-2 []
  (let [seed (atom 0)]
    (swap! seed inc)
    (Math/abs (Math/sin @seed))))
;=> #'clips.core/my-rand-2
(take 10 my-rand-coll)
;=>
;(0.8414709848078965
;  0.9092974268256817
;  0.1411200080598672
;  0.7568024953079282
;  0.9589242746631385
;  0.27941549819892586
;  0.6569865987187891
;  0.9893582466233818
;  0.4121184852417566
;  0.5440211108893698)
(repeatedly 10 my-rand-2)
;=>
;(0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965)
(defn make-my-rand-2' []
  (fn []
    (let [seed (atom 0)]
      (swap! seed inc)
      (Math/abs (Math/sin @seed)))))
;=> #'clips.core/make-my-rand-2'
(def my-rand-2' (make-my-rand-2'))
;=> #'clips.core/my-rand-2'
(repeatedly 10 my-rand-2')
;=>
;(0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965
;  0.8414709848078965)
; :thinking-face:
(defn make-my-rand-2'' []
  (let [seed (atom 0)]
    (fn []
      (swap! seed inc)
      (Math/abs (Math/sin @seed)))))
;=> #'clips.core/make-my-rand-2''
(def my-rand-2'' (make-my-rand-2''))
;=> #'clips.core/my-rand-2''
(repeatedly 10 my-rand-2'')
;=>
;(0.8414709848078965
;  0.9092974268256817
;  0.1411200080598672
;  0.7568024953079282
;  0.9589242746631385
;  0.27941549819892586
;  0.6569865987187891
;  0.9893582466233818
;  0.4121184852417566
;  0.5440211108893698)
