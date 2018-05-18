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
  [pattern op instr]
  (if-let [[_ a b] (and (string? instr) (re-find pattern instr))]
    {:op op :a (read-string a) :b (read-string b)}
    instr))

(defn- parse-rect
  "Parse a rect instruction or return identity.
  The instruction follows this pattern: `rect AxB`."
  [instr]
  (as-> (parse-screen-instr #"\brect\s+([0-9]+)x([0-9]+)\b" :rec instr) parsed
    (assoc parsed :wide (get parsed :a))
    (assoc parsed :tall (get parsed :b))
    (dissoc parsed :a)
    (dissoc parsed :b)))

(defn- parse-rot-row
  "Parse a rotate row instruction or return identity.
  The instruction follows this pattern: `rotate row y=A by B`."
  [instr]
  (as-> (parse-screen-instr #"\brotate\s+row\s+y=([0-9]+)\s+by\s+([0-9]+)\b" :rot-row instr) parsed
    (assoc parsed :row (get parsed :a))
    (assoc parsed :by (get parsed :b))
    (dissoc parsed :a)
    (dissoc parsed :b)))

(defn- parse-rot-col
  "Parse a rotate col instruction or return identity.
  The instruction follows this pattern: `rotate column x=A by B`."
  [instr]
  (as-> (parse-screen-instr #"\brotate\s+column\s+x=([0-9]+)\s+by\s+([0-9]+)\b" :rot-col instr) parsed
    (assoc parsed :col (get parsed :a))
    (assoc parsed :by (get parsed :b))
    (dissoc parsed :a)
    (dissoc parsed :b)))

(defmulti run-screen-instr
  "Execute a screen instruction."
  (fn [_ parsed] (:op parsed)))

(defmethod run-screen-instr :rec
  [screen {:keys [wide tall]}]
  (let [positions (for [col (range wide) row (range tall)]
                    [row col])]
    (reduce #(assoc-in %1 %2 \#) screen positions)))

(defmethod run-screen-instr :rot-row
  [screen {:keys [row by]}]
  (assoc screen row (->> (get screen row)
                         (repeat 2)
                         flatten
                         (drop (- wide by))
                         (take wide)
                         vec)))

(defmethod run-screen-instr :rot-col
  [screen {:keys [col by]}])

(-> "resources/aoc2016/screen-instructions"
    slurp
    clojure.string/split-lines
    (as-> lines (#_sequence transduce (comp
                                       (map parse-rect)
                                       (map parse-rot-row)
                                       (map parse-rot-col))
                           (completing run-screen-instr)
                           (empty-screen wide tall)
                           lines)))
