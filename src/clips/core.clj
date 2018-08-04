(ns clips.core
  (:require [clojure.core.async :as async]
            [spyscope.core]
            [clj-memory-meter.core :as mm]))

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

(defn read-file
  "Make file reader lazy-list."
  [file-path]
  (lazy-file (FileInputStream. file-path)))

(def f1 (read-file "resources/aoc2016/one"))
(def day9-file (read-file "resources/aoc2016/day9"))

(take 3 f1)
(reduce str (take 2 day9-file))

;;; Memory measurements
#_(with-open [rdr (clojure.java.io/reader "resources/aoc2016/day9")]
  (mm/measure rdr)) ; => 32.8 KB

#_(mm/measure f1) ; => 272 B
#_(mm/measure day9-file) ; => 272 B
#_(take 50 day9-file)
#_(mm/measure day9-file) ; => 5.7 KB
#_(count day9-file) ; => 14248
#_(mm/measure day9-file) ; => 1.5 MB

#_(mm/measure (slurp "resources/aoc2016/day9")) ; => 27.9 KB
#_(count (slurp "resources/aoc2016/day9"))

(mm/measure (read-file "resources/aoc2016/day9")) ; => 272 B
(mm/measure (take 50 (read-file "resources/aoc2016/day9"))) ; => 352 B
(mm/measure (reduce str (read-file "resources/aoc2016/day9"))) ; => 27.9 KB
(let [content (reduce str (read-file "resources/aoc2016/day9"))]
  (print content)
  (mm/measure content)) ; => 27.9 B
;; Clearly 27.9 B vs 1.5 MB is the difference between holding onto the
;; head of a lazy seq or not!

;;; AoC 2016 Day 9 Part 1
(let [day9 (read-file "resources/aoc2016/day9")])
