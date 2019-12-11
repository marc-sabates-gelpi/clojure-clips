(ns clips.aoc.2019.day2
  (:require [clojure.repl :refer :all]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(defn inc-pc
  "Return the next pc.
  In this implementation it increments the `pc` by 4."
  [pc]
  (+ 4 pc))

(defn str-coll->str-vec
  [s]
  (str "[" s "]"))

(defn make-machine
  "Return a machine.
  The machine is implemented with a map with keys:
  `code` -> Intcode vector
  `pc` -> Program counter
  `status` -> :standby :running :halted"
  [code noun verb]
  {:code   (-> code
               (assoc-in [1] noun)
               (assoc-in [2] verb))
   :status :standby
   :pc     0})

(defn- op-vals
  "Return the 2 operands vals from their position."
  [v [a b]]
  [(nth v a) (nth v b)])

(defn- op
  "Return the state for running op `f`."
  [{:keys [code pc] :as machine} f]
  (-> machine
      (assoc-in [:code (nth code (+ pc 3))] (apply f (op-vals code (subvec code (inc pc) (+ pc 3)))))
      (update :pc inc-pc)
      (assoc :status :running)))

(defmulti intcode-process (fn [{:keys [code pc]}] (nth code pc)))

(defmethod intcode-process 1
  [machine]
  (op machine +))

(defmethod intcode-process 2
  [machine]
  (op machine *))

(defmethod intcode-process 99
  [machine]
  (assoc machine :status :halted))

(defn run-machine
  "Return the state of a machine after it has reached status `:halted`."
  [initial-state]
  (loop [{:keys [status] :as state} initial-state]
    (if (#{:halted} status)
      state
      (recur (intcode-process state)))))

(defn part1
  [x]
  (-> (if (string? x)
        (-> x
            slurp
            str-coll->str-vec
            edn/read-string)
        x)
      (make-machine 12 2)
      run-machine))

#_(= (:code (part1 [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50])) [3500,9,10,70, 2,3,11,0, 99,30,40,50])
#_(= (:code (part1 [1, 0, 0, 0, 99])) [2,0,0,0,99])
#_(= (:code (part1 [2, 3, 0, 3, 99])) [2,3,0,6,99])
#_(= (:code (part1 [2, 4, 4, 5, 99, 0])) [2,4,4,5,99,9801])
#_(= (:code (part1 [1, 1, 1, 4, 99, 5, 6, 0, 99])) [30,1,1,4,2,5,6,0,99])
#_(part1 "resources/aoc2019/day2")

;;;; Part 2
(defn test-noun-verb-ranges
  "Return a collection of maps with keys `n`, `v`, `result`."
  [max-noun max-verb memory]
  (for [n (range (inc max-noun)) v (range (inc max-verb))
        :let [res (-> memory (make-machine n v) run-machine)]]
    {:n      n
     :v      v
     :result (get-in res [:code 0])}))

(defn part2
  []
  (->> "resources/aoc2019/day2"
       slurp
       str-coll->str-vec
       edn/read-string
       (test-noun-verb-ranges 100 100)
       (filter (comp #{19690720} :result))))

;(part2)
;=> ({:n 80, :v 18, :result 19690720})

