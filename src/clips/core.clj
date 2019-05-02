(ns clips.core
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clj-memory-meter.core :as mm]
            [spyscope.core]))

;;;; Session 25/04/2019

(use '[clojure.repl])
(doc apropos)

;; array map
(map (fn [el] el) {:a 1 :b 2});; => ([:a 1] [:b 2])
(map identity {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8})
;; => ([:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6] [:g 7] [:h 8])
(type {:a 1 :b 2})
;; => clojure.lang.PersistentArrayMap

;; hash map
(map identity {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9});; => ([:e 5] [:g 7] [:c 3] [:h 8] [:b 2] [:d 4] [:f 6] [:i 9] [:a 1])
(type {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9})
;; => clojure.lang.PersistentHashMap

;;destructure with namespaced keys
(let [{:my-ns/keys [a b] :or {a 99 b 98}} #:my-ns{:a 1}] [a b]);; => [1 98]


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

(macroexpand `#(* %1 %9));; => (fn* [p1__13404__13413__auto__ p2__13406__13414__auto__ p3__13407__13415__auto__ p4__13408__13416__auto__ p5__13409__13417__auto__ p6__13410__13418__auto__ p7__13411__13419__auto__ p8__13412__13420__auto__ p9__13405__13421__auto__] (clojure.core/* p1__13404__13413__auto__ p9__13405__13421__auto__))

(macroexpand `#((+ 1 2)))
;; => (fn* [] ((clojure.core/+ 1 2)))

(macroexpand `#(do (+ 1 2) (* 1 2)));; => (fn* [] (do (clojure.core/+ 1 2) (clojure.core/* 1 2)))

*compiler-options*;; => nil
*data-readers*;; => {dbg #'cider.nrepl.middleware.debug/debug-reader, break #'cider.nrepl.middleware.debug/breakpoint-reader, light #'cider.nrepl.middleware.enlighten/light-reader}
clojure.core/default-data-readers;; => {uuid #'clojure.uuid/default-uuid-reader, inst #'clojure.instant/read-instant-date}

(read-string "#=(+ 1 1)");; => 2
(binding [*read-eval* false] (read-string "#=(+ 1 1)"))
*e;; => #error {
 :cause "EvalReader not allowed when *read-eval* is false."
 :via
 [{:type java.lang.RuntimeException
   :message "EvalReader not allowed when *read-eval* is false."
   :at [clojure.lang.Util runtimeException "Util.java" 221]}]
 :trace
 [[clojure.lang.Util runtimeException "Util.java" 221]
  [clojure.lang.LispReader$EvalReader invoke "LispReader.java" 1297]
  [clojure.lang.LispReader$DispatchReader invoke "LispReader.java" 853]
  [clojure.lang.LispReader read "LispReader.java" 285]
  [clojure.lang.LispReader read "LispReader.java" 216]
  [clojure.lang.LispReader read "LispReader.java" 205]
  [clojure.lang.RT readString "RT.java" 1874]
  [clojure.lang.RT readString "RT.java" 1869]
  [clojure.core$read_string invokeStatic "core.clj" 3815]
  [clojure.core$read_string invoke "core.clj" 3805]
  [clips.core$eval13448 invokeStatic "NO_SOURCE_FILE" 55]
  [clips.core$eval13448 invoke "NO_SOURCE_FILE" 55]
  [clojure.lang.Compiler eval "Compiler.java" 7176]
  [clojure.lang.Compiler eval "Compiler.java" 7131]
  [clojure.core$eval invokeStatic "core.clj" 3214]
  [clojure.core$eval invoke "core.clj" 3210]
  [clojure.main$repl$read_eval_print__9068$fn__9071 invoke "main.clj" 414]
  [clojure.main$repl$read_eval_print__9068 invoke "main.clj" 414]
  [clojure.main$repl$fn__9077 invoke "main.clj" 435]
  [clojure.main$repl invokeStatic "main.clj" 435]
  [clojure.main$repl doInvoke "main.clj" 345]
  [clojure.lang.RestFn applyTo "RestFn.java" 137]
  [clojure.core$apply invokeStatic "core.clj" 665]
  [clojure.core$apply invoke "core.clj" 660]
  [refactor_nrepl.ns.slam.hound.regrow$wrap_clojure_repl$fn__9642 doInvoke "regrow.clj" 18]
  [clojure.lang.RestFn invoke "RestFn.java" 1523]
  [nrepl.middleware.interruptible_eval$evaluate invokeStatic "interruptible_eval.clj" 79]
  [nrepl.middleware.interruptible_eval$evaluate invoke "interruptible_eval.clj" 55]
  [nrepl.middleware.interruptible_eval$interruptible_eval$fn__935$fn__939 invoke "interruptible_eval.clj" 142]
  [clojure.lang.AFn run "AFn.java" 22]
  [nrepl.middleware.session$session_exec$main_loop__1036$fn__1040 invoke "session.clj" 171]
  [nrepl.middleware.session$session_exec$main_loop__1036 invoke "session.clj" 170]
  [clojure.lang.AFn run "AFn.java" 22]
  [java.lang.Thread run "Thread.java" 748]]}

(macroexpand `(-> 2 inc (* 2)));; => (clojure.core/* (clojure.core/inc 2) 2)
