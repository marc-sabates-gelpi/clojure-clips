(ns clips.aoc.2019.day5
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [taoensso.timbre :refer [spy debug]]))

(defn make-machine
  "Return a machine.
  The machine is implemented with a map with keys:
  `code` -> Intcode vector
  `pc` -> Program counter
  `status` -> :standby :running :halted"
  [code]
  {:code   code
   :status :standby
   :pc     0})

(defn get-param-by-value
  [{:keys [code pc] :as _machine} i]
  (nth (drop pc code) i))

(defn get-param-by-ref
  "Return the value in the position for parami in `[op param1 param2 ... paramn]`."
  [{:keys [code pc] :as _machine} i]
  ;; We drop pc to omit previous instrs.
  ;; Notice that 1 <= i <= n, so we avoid op using i with `nth`
  (nth code (nth (drop pc code) i)))

(defn get-result-param
  "Return the result param in `[op ... skin-n ... r]`."
  [{:keys [code pc] :as _machine} skip-n]
  ;; the extra skip is for the op
  (nth code (+ pc skip-n 1)))

(defn common-instr-update
  "Update :pc and :status."
  [machine inst-size]
  (-> machine
      (update :pc (partial + inst-size))
      (assoc :status :running)))

(defn- exp
  [n p]
  (apply * (repeat p n)))

(defn- mode-digits
  [n param-num]
  (rem (int (/ n (exp 10 (inc param-num)))) (exp 10 param-num)))

(defn get-param-f
  "Return apropiate fn to get the param n.
  The fn depends on the mode defined in the upper digits of the op code."
  [{:keys [pc code] :as _machine} param-num]
  (case (mode-digits (nth code pc) param-num)
    0 get-param-by-ref
    1 get-param-by-value
    get-param-by-ref))

(defn get-param
  [machine param-num]
  ((get-param-f machine param-num) machine param-num))

(defmulti intcode-process (fn [{:keys [code pc]}] (rem (nth code pc) 100)))

(defmethod intcode-process 1
  [machine]
  (-> machine
      (assoc-in [:code (get-result-param machine 2)] (apply + (mapv (partial get-param machine) [1 2])))
      (common-instr-update 4)))

(defmethod intcode-process 2
  [machine]
  (-> machine
      (assoc-in [:code (get-result-param machine 2)] (apply * (mapv (partial get-param machine) [1 2])))
      (common-instr-update 4)))

(defmethod intcode-process 3
  [machine]
  (-> machine
      (assoc-in [:code (get-result-param machine 0)] (edn/read-string (do
                                                                        (print "$ Input: ")
                                                                        (flush)
                                                                        (read-line))))
      (common-instr-update 2)))

(defmethod intcode-process 4
  [machine]
  (println (get-param machine 1))
  (common-instr-update machine 2))

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
        (mapv edn/read-string (-> x slurp (string/split #",")))
        x)
      make-machine
      run-machine))

;(do (part1 "resources/aoc2019/day5") nil)
;$ Input: 1
;0
;0
;0
;0
;0
;0
;0
;0
;0
;5182797
;=> nil


;;;; Part 2
(defmethod intcode-process 5
  [machine]
  (if-not (zero? (get-param machine 1))
    (-> machine
        (assoc :pc (get-param machine 2))
        (assoc :status :running))
    (common-instr-update machine 3)))

(defmethod intcode-process 6
  [machine]
  (if (zero? (get-param machine 1))
    (-> machine
        (assoc :pc (get-param machine 2))
        (assoc :status :running))
    (common-instr-update machine 3)))

(defmethod intcode-process 7
  [machine]
  (-> machine
      (assoc-in [:code (get-result-param machine 2)] (if (< (get-param machine 1) (get-param machine 2)) 1 0))
      (common-instr-update 4)))

(defmethod intcode-process 8
  [machine]
  (-> machine
      (assoc-in [:code (get-result-param machine 2)] (if (= (get-param machine 1) (get-param machine 2)) 1 0))
      (common-instr-update 4)))

(def part2 part1)

;(do (part2 "resources/aoc2019/day5") nil)
;$ Input: 5
;12077198
;=> nil
