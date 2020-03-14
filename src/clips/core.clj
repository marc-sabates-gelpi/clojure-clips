(ns clips.core
  (:require #_[clojure.core.async :as async]
    [clj-memory-meter.core :as mm]
    [clojure.edn :as edn]
    [clojure.repl :refer [:all]]
    [clojure.string :as string]
    #_[spyscope.core]
    [lambdaisland.regal :refer [regex]]))

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

(def r [:cat
        "</p>"
        [:* \s]
        "<p>"])
;(string/replace #"(?i)</p>\s*<p>" "</p>\n<p>")
(string/replace "<p>This is a test</p><p>And this is a second paragraph</p>" (regex r) "</p>\n<p>")
;=> "<p>This is a test</p>\n<p>And this is a second paragraph</p>"
(string/replace "<p>This is a test</p>    <p>And this is a second paragraph</p>" (regex r) "</p>\n<p>")
;=> "<p>This is a test</p>    <p>And this is a second paragraph</p>"
(string/replace "<p>This is a test</p>    <p>And this is a second paragraph</p>"
                (regex [:cat
                        "</p>"
                        [:* \space]
                        "<p>"])
                "</p>\n<p>")
;=> "<p>This is a test</p>\n<p>And this is a second paragraph</p>"

;; Session 18/02/2020
;; dharrigan's code:
(let [app-config {}
      cleanup-stagnant-data (fn [table _config]
                              (Thread/sleep 3000)
                              (array-map table :successful))
      five-minutes-ms (* 5 60 1000)
      workers (pmap #(cleanup-stagnant-data % app-config) [:funky-table-1 :funky-table-2 :funky-table-3])]
  (map #(deref % five-minutes-ms :failure) workers))
;Error printing return value (ClassCastException) at clojure.core/deref-future (core.clj:2298).
;class clojure.lang.PersistentArrayMap cannot be cast to class java.util.concurrent.Future (clojure.lang.PersistentArrayMap is in unnamed module of loader 'app'; java.util.concurrent.Future is in module java.base of loader 'bootstrap')

(get #{1 2 3} 1 :not-found)
;=> 1
(get #{1 2 3} 0 :not-found)
;=> :not-found
(#{1 2 3} 1)
;=> 1
(#{1 2 3} 0 :not-found)
;Execution error (ArityException) at clips.core/eval2316 (form-init12214707321052361609.clj:1).
;Wrong number of args (2) passed to: clojure.lang.PersistentHashSet

;;;; Session 19/02/2020
(defn ls1 [seed]
  (println "---> ls1 " seed)
  (when (pos? seed)
    (cons seed (ls1 (dec seed)))))

(defn ls2 [seed]
  (println "---> ls2 " seed)
  (when (pos? seed)
    (cons seed (ls2 (dec seed)))))

(def ls-test (lazy-seq (ls1 15) (ls2 20)))

(take 1 ls-test)
;---> ls1  15
;---> ls1  14
;---> ls1  13
;---> ls1  12
;---> ls1  11
;---> ls1  10
;---> ls1  9
;---> ls1  8
;---> ls1  7
;---> ls1  6
;---> ls1  5
;---> ls1  4
;---> ls1  3
;---> ls1  2
;---> ls1  1
;---> ls1  0
;---> ls2  20
;---> ls2  19
;---> ls2  18
;---> ls2  17
;---> ls2  16
;---> ls2  15
;---> ls2  14
;---> ls2  13
;---> ls2  12
;---> ls2  11
;---> ls2  10
;---> ls2  9
;---> ls2  8
;---> ls2  7
;---> ls2  6
;---> ls2  5
;---> ls2  4
;---> ls2  3
;---> ls2  2
;---> ls2  1
;---> ls2  0
;=> (20)

(def ls-test (lazy-cat (ls1 10) (ls2 20)))  ;; `lazy-cat` you idiot, not `lazy-seq` :grinning:

(take 1 ls-test)
;---> ls1  10
;---> ls1  9
;---> ls1  8
;---> ls1  7
;---> ls1  6
;---> ls1  5
;---> ls1  4
;---> ls1  3
;---> ls1  2
;---> ls1  1
;---> ls1  0
;=> (10)

(into-array [1 2 3])
;=> #object["[Ljava.lang.Long;" 0xf875d12 "[Ljava.lang.Long;@f875d12"]
(into-array [true false true])
;=> #object["[Ljava.lang.Boolean;" 0x46408b05 "[Ljava.lang.Boolean;@46408b05"]
(aset-boolean (into-array [true false true]) 1 2 false)
;Execution error (IllegalArgumentException) at java.lang.reflect.Array/setBoolean (Array.java:-2).
;Argument is not an array
(let [bs (boolean-array 3)]
  (aset-boolean bs 2 true)
  (vec bs))
;=> [false false true]
;:thinking_face:


;;;; Session 02/03/2020

(not= 1 1 2)
;=> true
; Watch out because it doesn't mean all different!

;;;; Session 03/03/2020

;; distinct-with
(defn distinct-with
  [f coll]
  (vals (zipmap (map f coll) coll)))

;(distinct-with odd? (range 10))
;=> (8 9)
;(map odd? (range 10))
;=> (false true false true false true false true false true)

(defn distinct-with2
  [f coll]
  (map first (vals (group-by f coll))))

;(distinct-with2 odd? (range 10))
;=> (0 1)

;(tree-seq seq? identity '((1 2 (3)) (4)))
;=> (((1 2 (3)) (4)) (1 2 (3)) 1 2 (3) 3 (4) 4)
;(tree-seq (constantly false) identity '((1 2 (3)) (4)))
;=> (((1 2 (3)) (4)))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, removing any elements that
  return duplicate values when passed to a function f."
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result x]
          (let [fx (f x)]
            (if (contains? @seen fx)
              result
              (do (vswap! seen conj fx)
                  (rf result x)))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [fx (f x)]
                         (if (contains? seen fx)
                           (recur (rest s) seen)
                           (cons x (step (rest s) (conj seen fx)))))))
                   xs seen)))]
     (step coll #{}))))

;(distinct-by odd? (range 10))
;=> (0 1)

;;;; Session 12/03/2020
(clojure.string/join '(:a "b" \c ["d"]))
;=> ":abc[\"d\"]"
(clojure.string/join " " '(:a "b" \c ["d"]))
;=> ":a b c [\"d\"]"

(let [a (atom 0)]
  (defn test-my-enclosure [x]
    (swap! a + x)))
;=> #'user/test-my-enclosure
(test-my-enclosure 2)
;=> 2
(test-my-enclosure 3)
;=> 5
(test-my-enclosure 3)
;=> 8

4e-2
;=> 0.04

;;;; Session 13/03/2020
(ns my.test (:require [lambdaisland.regal :refer [regex]]
                      [clojure [edn :as edn] repl [string :as str]]))
;=> nil
*ns*
;=> #object[clojure.lang.Namespace 0x22c3ed82 "my.test"]
regex
;=> #object[lambdaisland.regal$regex 0x23caefbd "lambdaisland.regal$regex@23caefbd"]
doc
;Syntax error compiling at (/tmp/form-init4100735502028150817.clj:1:349).
;Unable to resolve symbol: doc in this context
clojure.repl/doc
;Syntax error compiling at (/tmp/form-init4100735502028150817.clj:1:349).
;Can't take value of a macro: #'clojure.repl/doc
apropos
;Syntax error compiling at (/tmp/form-init4100735502028150817.clj:1:349).
;Unable to resolve symbol: apropos in this context
clojure.repl/apropos
;=> #object[clojure.repl$apropos 0x53518d61 "clojure.repl$apropos@53518d61"]
edn
;Syntax error compiling at (/tmp/form-init4100735502028150817.clj:1:349).
;Unable to resolve symbol: edn in this context
edn/read-string
;=> #object[clojure.edn$read_string 0x3880aefa "clojure.edn$read_string@3880aefa"]
str/reverse
;=> #object[clojure.string$reverse 0x7e3bc440 "clojure.string$reverse@7e3bc440"]
(ns my.test (:require [lambdaisland.regal :refer [regex]]
                      '(clojure [edn :as edn] repl [string :as str])))
;Syntax error macroexpanding clojure.core/ns at (/tmp/form-init4100735502028150817.clj:1:1).
;((:require [lambdaisland.regal :refer [regex]] (quote (clojure [edn :as edn] repl [string :as str])))) - failed: Extra input spec: :clojure.core.specs.alpha/ns-form
(ns my.test (:require [lambdaisland.regal :refer [regex]]
                      (clojure [edn :as edn] repl [string :as str])))
;=> nil
(ns my.test (:require [lambdaisland.regal :refer [regex]]
                      (clojure [edn :as edn2] [repl :refer [:all]] [string :as str2])))
;Syntax error macroexpanding clojure.core/ns at (/tmp/form-init4100735502028150817.clj:1:1).
;((:require [lambdaisland.regal :refer [regex]] (clojure [edn :as edn2] [repl :refer [:all]] [string :as str2]))) - failed: Extra input spec: :clojure.core.specs.alpha/ns-form
(ns my.test (:require [lambdaisland.regal :refer [regex]]
                      (clojure [edn :as edn2] [repl :as rpl] [string :as str2])))
;=> nil
(ns my.test (:require [lambdaisland.regal :refer [regex]]
                      (clojure [edn :as edn2 :refer [read-string]] [repl :as rpl] [string :as str2])))
;WARNING: read-string already refers to: #'clojure.core/read-string in namespace: my.test, being replaced by: #'clojure.edn/read-string
;=> nil
(ns my.test (:require [lambdaisland.regal :refer [regex]]
                      (clojure [edn :as edn2 :refer [read-string]] [repl :as rpl :refer [apropos]] [string :as str2])))
;WARNING: read-string already refers to: #'clojure.core/read-string in namespace: my.test, being replaced by: #'clojure.edn/read-string
;=> nil
(ns my.test (:require [lambdaisland.regal :refer [regex]]
                      (clojure [edn :as edn2 :refer [read-string]] [repl :as rpl :refer :all] [string :as str2])))
;WARNING: read-string already refers to: #'clojure.core/read-string in namespace: my.test, being replaced by: #'clojure.edn/read-string
;=> nil
str/join
;=> #object[clojure.string$join 0x4c909edc "clojure.string$join@4c909edc"]
str2/join
;=> #object[clojure.string$join 0x4c909edc "clojure.string$join@4c909edc"]

;;;; Session 14/03/2020
(defn surname-first [[name middlename surname]]
  (str/join ", " [surname (str/join " " [name middlename])]))
;=> #'user/surname-first
(surname-first ["Guy" "Lewis" "Steele"])
;=> "Steele, Guy Lewis"
(surname-first ["Guy" nil "Steele"])
;=> "Steele, Guy "
(surname-first ["Guy" nil nil])
;=> ", Guy "
(surname-first [nil nil "Steele"])
;=> "Steele,  "

(defn surname-first [[name middlename surname]]
  (apply str (interpose ", " [surname (apply str (interpose " " [name middlename]))])))
;=> #'user/surname-first
(surname-first ["Guy" "Lewis" "Steele"])
;=> "Steele, Guy Lewis"
(surname-first ["Guy" nil "Steele"])
;=> "Steele, Guy "
(surname-first ["Guy" nil nil])
;=> ", Guy "
(surname-first [nil nil "Steele"])
;=> "Steele,  "

(defn surname-first [[name middlename surname]]
  (format "%s, %s %s" surname name middlename))
;=> #'user/surname-first
(surname-first ["Guy" "Lewis" "Steele"])
;=> "Steele, Guy Lewis"
(surname-first ["Guy" nil "Steele"])
;=> "Steele, Guy null"
(surname-first ["Guy" nil nil])
;=> "null, Guy null"
(surname-first [nil nil "Steele"])
;=> "Steele, null null"

;; Joy of Clojure proposal
(defn surname-first [[name middlename surname]]
  (str surname ", " name " " middlename))
;=> #'user/surname-first
(surname-first ["Guy" "Lewis" "Steele"])
;=> "Steele, Guy Lewis"
(surname-first ["Guy" nil "Steele"])
;=> "Steele, Guy "
(surname-first ["Guy" nil nil])
;=> ", Guy "
(surname-first [nil nil "Steele"])
;=> "Steele,  "

(let [[h & tail] "hello"]
  {:head h
   :tail tail})
;=> {:head \h, :tail (\e \l \l \o)}

(require '[clojure.walk :refer [macroexpand-all]])
;=> nil
(macroexpand-all '(fn [[x & more]] x))
;=>
;(fn*
;  ([p__1905]
;   (let*
;     [vec__1906
;      p__1905
;      seq__1907
;      (clojure.core/seq vec__1906)
;      first__1908
;      (clojure.core/first seq__1907)
;      seq__1907
;      (clojure.core/next seq__1907)
;      x
;      first__1908
;      more
;      seq__1907]
;     x)))

(macroexpand-all '(let [[fst scd] (re-matcher #"[0-9]+" "19 78 4d no-num F1")]
                    {:first  fst
                     :second scd}))
;=>
;(let*
;  [vec__1913
;   (re-matcher #"[0-9]+" "19 78 4d no-num F1")
;   fst
;   (clojure.core/nth vec__1913 0 nil)
;   scd
;   (clojure.core/nth vec__1913 1 nil)]
;  {:first fst, :second scd})


(def me-matcher (re-matcher #"([0-9]+)+" "19 78 4d no-num F1"))
;=> #'user/me-matcher
(re-find me-matcher)
;=> ["19" "19"]
(nth me-matcher 0)
;=> "19"
(re-groups me-matcher)
;=> ["19" "19"]
(nth me-matcher 1)
;=> "19"
(nth me-matcher 2 nil)
;=> nil
(let [[fst scd] me-matcher]
  {:first  fst
   :second scd})
;=> {:first "19", :second nil}
(let [me-matcher2 (re-matcher #"([0-9]+)" "19 78 4d nom-num F1")
      [fst scd] me-matcher2]
  {:first  fst
   :second scd})
;Execution error (IllegalStateException) at java.util.regex.Matcher/group (Matcher.java:645).
;No match found
(let [me-matcher2 (re-matcher #"([0-9]+)" "19 78 4d nom-num F1")
      _ (re-find me-matcher2)
      [fst scd] me-matcher2]
  {:first  fst
   :second scd})
;=> {:first "19", :second nil}
(let [me-matcher2 (re-matcher #"([0-9]+)" "19 78 4d nom-num F1")
      _ (re-find me-matcher2)
      _ (re-find me-matcher2)
      [fst scd] me-matcher2]
  {:first  fst
   :second scd})
;=> {:first "78", :second nil}

;; no groups
(let [me-matcher2 (re-matcher #"[0-9]+" "19 78 4d nom-num F1")
      _ (re-find me-matcher2)
      _ (re-find me-matcher2)
      [fst scd] me-matcher2]
  {:first  fst
   :second scd})
;=> {:first nil, :second nil}

;; no capturing groups
(let [me-matcher2 (re-matcher #"(?:[0-9]+)" "19 78 4d nom-num F1")
      _ (re-find me-matcher2)
      _ (re-find me-matcher2)
      [fst scd] me-matcher2]
  {:first  fst
   :second scd})
;=> {:first nil, :second nil}

;; I am a bit tired of these matchers
;; This doesn't seem to make any sense to me..
(macroexpand-all '(let [person-matcher (re-matcher #"(\w+)\s(\w+)\s(\d+)\s?" "John Smith 21\nJane Doe 42")
                        _              (re-find person-matcher)
                        [_ name surname age] person-matcher]
                    {:name    name
                     :surname surname
                     :age     age}))
;=>
;(let*
;  [person-matcher
;   (re-matcher #"(\w+)\s(\w+)\s(\d+)\s?" "John Smith 21\nJane Doe 42")
;   _
;   (re-find person-matcher)
;   vec__2152
;   person-matcher
;   _
;   (clojure.core/nth vec__2152 0 nil)
;   name
;   (clojure.core/nth vec__2152 1 nil)
;   surname
;   (clojure.core/nth vec__2152 2 nil)
;   age
;   (clojure.core/nth vec__2152 3 nil)]
;  {:name name, :surname surname, :age age})
(let [person-matcher (re-matcher #"(\w+)\s(\w+)\s(\d+)\s?" "John Smith 21\nJane Doe 42")
      _              (re-find person-matcher)
      [_ name surname age] person-matcher]
  {:name    name
   :surname surname
   :age     age})
;=> {:name "John", :surname "Smith", :age nil}
(let [person-matcher (re-matcher #"(\w+)\s(\w+)\s(\d+)\s?" "John Smith 21\nJane Doe 42")
      _              (re-find person-matcher)
      [_ name surname age] person-matcher]
  #_{:name    name
     :surname surname
     :age     age}
  (nth person-matcher 3))
;=> "21"

;; I give up.. But my point was proved.. One can use matchers in vec destructuring
