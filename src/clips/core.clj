(ns clips.core
  (:require [clojure.core.async :as async]
            [spyscope.core]))

;;;; Session 03/08/2018

(->> '({:id 1 :val 1} {:id 2 :val 2})
     (group-by :id)
     (reduce (fn [acc [k v]] (assoc acc k (first v))) {}))

;;; Challenge: Read a file as lazily as possible

;; Read chars 1 at a time
(import '(java.io FileInputStream))

(defn read-char
  "Read a single char from `InputStream`."
  [is]
  (let [c (.read is)]
    (if-not (neg? c)
      (str (char c))
      (.close is))))

(with-open [fis (FileInputStream. "resources/aoc2016/day9")]
  (let [fst-char (read-char fis)
        scnd-char (read-char fis)]
    (print (format "Read %d char: %s" (count fst-char) fst-char)
           (format "Read %d char: %s" (count scnd-char) scnd-char))))

;; Create a lazy seq of chars (as strings)
(defn lazy-file
  "Return a lazy seq from the input stream `is`."
  [is]
  (when-let [c (read-char is)]
    (lazy-seq (cons c (lazy-file is)))))

(defn make-file-reader
  "Make file reader lazy-list."
  [file-path]
  (lazy-file (FileInputStream. file-path)))

(def f1 (make-file-reader "resources/aoc2016/one"))
(def day9-file (make-file-reader "resources/aoc2016/day9"))

(take 3 f1)
(reduce str (take 2 day9-file))

;;; AoC 2016 Day 9 Part 1
(with-open [rdr (clojure.java.io/reader "resources/aoc2016/day9")]
  (nth rdr 2))
