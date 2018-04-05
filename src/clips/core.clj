(ns clips.core
  (:require [clojure.core.async :as async]
            [spyscope.core]))
;;; Session 01/04/2018

(reduce + 0 (range 10))

(def ^:private some-primes '(3 5 7 11 13 17 19 23))

(defn- my-processor-f
  "Function that processes an element to a collection."
  [x]
  (when (< x 5)
    (map #(* % x) some-primes)))

(defn- my-reduce-f
  "Function that reduces to itself."
  [acc el]
  (printf "Accumulated: %s. Current element: %s\n" acc el)
  (reduce my-reduce-f (conj acc el) (my-processor-f el)))

(reduce my-reduce-f #{} (my-processor-f 1))

;;; AoC 2016 Day 8 Part 1

(def ^:private wide 50)
(def ^:private tall 6)

(defn- empty-screen
  "Create a `wide` x `tall` empty screen."
  [wide tall]
  {:pre [(pos? wide) (pos? tall)]}
  (let [row (vec (repeat wide \.))]
    (vec
     (for [r (range tall)]
       row))))

(defn- parse-screen-instr
  "Parse a screen instruction.
  Try to match `pattern` and if so return screen op `op`,
  otherwsie return identity."
  [pattern op s]
  (if-let [[_ a b] (and (string? s) (re-find pattern s))]
    {:op op :a (read-string a) :b (read-string b)}
    s))

(defn- parse-rect
  "Parse a rect instruction or return identity.
  The instruction follows this pattern: `rect AxB`."
  [s]
  (parse-screen-instr #"\brect\s+([0-9]+)x([0-9]+)\b" :rec s))

(defn- parse-rot-row
  "Parse a rotate row instruction or return identity.
  The instruction follows this pattern: `rotate row y=A by B`."
  [s]
  (parse-screen-instr #"\brotate\s+row\s+y=([0-9]+)\s+by\s+([0-9]+)\b" :rot-row s))

(defn- parse-rot-col
  "Parse a rotate col instruction or return identity.
  The instruction follows this pattern: `rotate column x=A by B`."
  [s]
  (parse-screen-instr #"\brotate\s+column\s+x=([0-9]+)\s+by\s+([0-9]+)\b" :rot-col s))

(-> "resources/aoc2016/screen-instructions"
    slurp
    clojure.string/split-lines
    (as-> lines (sequence #_transduce (comp
                                       (map parse-rect)
                                       (map parse-rot-row)
                                       (map parse-rot-col))
                           #_(completing run-screen-instr)
                           #_(empty-screen wide tall)
                           lines)))
