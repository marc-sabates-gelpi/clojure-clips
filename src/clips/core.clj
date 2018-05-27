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
  [pattern op instr label-a label-b]
  (if-let [[_ a b] (and (string? instr) (re-find pattern instr))]
    {:op op label-a (read-string a) label-b (read-string b)}
    instr))

(defn- parse-rect
  "Parse a rect instruction or return identity.
  The instruction follows this pattern: `rect AxB`."
  [instr]
  (parse-screen-instr #"\brect\s+([0-9]+)x([0-9]+)\b" :rec instr :wide :tall))

(defn- parse-rot-row
  "Parse a rotate row instruction or return identity.
  The instruction follows this pattern: `rotate row y=A by B`."
  [instr]
  (parse-screen-instr #"\brotate\s+row\s+y=([0-9]+)\s+by\s+([0-9]+)\b" :rot-row instr :row :by))

(defn- parse-rot-col
  "Parse a rotate col instruction or return identity.
  The instruction follows this pattern: `rotate column x=A by B`."
  [instr]
  (parse-screen-instr #"\brotate\s+column\s+x=([0-9]+)\s+by\s+([0-9]+)\b" :rot-col instr :col :by))

(defn- shift-right
  "Shift a collection `n` positions to the right.
  It wraps on `len` and returns a vector."
  [coll n len]
  (->> coll
       (repeat 2)
       flatten
       (drop (- len n))
       (take len)
       vec))

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
  (assoc screen row (shift-right (get screen row) by wide)))

(defmethod run-screen-instr :rot-col
  [screen {:keys [col by]}]
  (let [column (map #(get-in screen [% col]) (range tall))
        shifted (shift-right column by tall)]
    (reduce-kv #(assoc-in %1 [%2 col] %3) screen shifted)))

(-> "resources/aoc2016/screen-instructions"
    slurp
    clojure.string/split-lines
    (as-> lines (transduce (comp
                            (map parse-rect)
                            (map parse-rot-row)
                            (map parse-rot-col))
                           (completing run-screen-instr)
                           (empty-screen wide tall)
                           lines))
    flatten
    (#(filter #{\#} %))
    count)

;;; some-> & cond->

(some-> {}
        (assoc :new-key :new-val)
        {} (assoc :another-key :another-val))

(some-> {}
        (assoc :new-key :new-val)
        (assoc :another-key :another-val))

(let [name :name surname nil tel :tel]
  (cond-> {}
    name (assoc :name name)
    surname (assoc :surname surname)
    tel (assoc :tel tel)
    true (assoc :timestamp (java.util.Date.))))

;;; Idiomatic clojure

(let [k :name0]
  (some (comp #{k} :field1) '({:field1 :name1 :field2 :value1} {:field1 :name0 :field2 :value0})))

(let [k :name0]
  (some (fn [{:keys [field1] :as whole}] (when (some #{k} (list field1)) whole)) '({:field1 :name1 :field2 :value1} {:field1 :name0 :field2 :value0})))

(let [k :name0]
  (some (fn [{:keys [nspace/field1] :as whole}] (when (some #{k} (list field1)) whole)) '({:nspace/field1 :name1 :nspace/field2 :value1} {:nspace/field1 :name0 :nspace/field2 :value0})))

(defmacro some-in [k v coll]
  "`some` val `v` in `coll` of maps with key `k`."
  `(some (fn [{:keys [~k] :as whole#}]
           (when (some #{~v} (list ~(symbol (.getName k))))
                  whole#)) ~coll))

(let [k :name0]
  (some-in field1 k '({:field1 :name1 :field2 :value1} {:field1 :name0 :field2 :value0})))

(let [k :name0]
  (some-in nspace/field1 k '({:nspace/field1 :name1 :nspace/field2 :value1} {:nspace/field1 :name0 :nspace/field2 :value0})))

(let [name-looked-for :name2
      responses '({:application.response/greenhouse-name :name0 :application.response/answer "Answer 0"} {:application.response/greenhouse-name :name1 :application.response/answer "Answer 1"} {:application.response/greenhouse-name :name2 :application.response/answer "Answer 2"})]
  [(->> responses
         (filter #(= name-looked-for (:application.response/greenhouse-name %)))
         first
         :application.response/answer)

   (->> responses
        (some (fn [{:keys [application.response/greenhouse-name] :as whole}]
                (when (some #{name-looked-for} (list greenhouse-name))
                  whole)))
        :application.response/answer)

   (->> responses
        (some-in application.response/greenhouse-name name-looked-for)
        :application.response/answer)])
