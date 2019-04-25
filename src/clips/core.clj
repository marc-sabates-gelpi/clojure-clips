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
