(ns clips.core
  (:require [clojure.core.async :as async]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clj-memory-meter.core :as mm]
            [spyscope.core]))

;;; AoC 2016 Day 9 Part 1
(defn uncompress
  [in]
  (if-not (= \( (first in))
    [(subs in 1 (count in)) (subs in 0 1)]
    (let [cmd (re-find #"\([0-9]+[Xx][0-9]+\)" in)
          [len times] (map clojure.edn/read-string (-> cmd
                                                       (string/replace #"[\(\)]" "")
                                                       (string/split #"[Xx]")))
          remaining-in (subs in (+ (count cmd) len) (count in))
          pattern (subs in (count cmd) (+ (count cmd) len))
          out (apply str (take (* len times) (cycle pattern)))]
      [remaining-in out])))

(defn day9-1
  [in]
  (let [input (if (string/index-of in "resources/") (slurp in) in)]
    (loop [compressed input uncompressed nil]
      (if (seq compressed)
        (let [[updated-compressed new-chunk] (uncompress compressed)]
          (recur updated-compressed (str uncompressed new-chunk)))
        uncompressed))))

(defmacro str-and-count
  [form#]
  `((juxt identity count) ~form#))

(str-and-count (day9-1 "ADVENT"))
(str-and-count (day9-1 "A(1x5)BC"))
(str-and-count (day9-1 "(3x3)XYZ"))
(str-and-count (day9-1 "A(2x2)BCD(2x2)EFG"))
(str-and-count (day9-1 "(6x1)(1x3)A"))
(str-and-count (day9-1 "X(8x2)(3x3)ABCY"))
(str-and-count (day9-1 "resources/aoc2016/day9"))

;;; Aoc 2016 Day 9 Part 2
(defn text-or-nil
  "Return the `text` if it is not blank; return nil otherwise."
  [text]
  (when (seq text)
    text))

(defn marker
  "Return a map with `times`, `text` and `remainder` of a marker if it exists.
  otherwise return a map with `pre`(next potential marker) and `remainder`."
  [text]
  (if-let [[marker len times] (re-find #"^\(([0-9]+)[Xx]([0-9]+)\)" text)]
    (let [len (edn/read-string len)
          times (edn/read-string times)
          rest-len (+ (count marker) len)
          remainder (text-or-nil (subs text rest-len))]
      (cond-> {:times times
               :text (subs text (count marker) rest-len)}
         remainder (assoc :remainder remainder)))
    (let [pre (-> text
                  (string/split #"\(")
                  first
                  text-or-nil)
          remainder (-> text
                        (subs (count pre))
                        text-or-nil)]
      (cond-> {}
        pre (assoc :pre pre)
        remainder (assoc :remainder remainder)))))

(defn deep-process
  "Expand the first marker down to plain text."
  [input]
  (loop [remainder input output {}]
    (if (seq remainder)
      (let [{:keys [text times remainder pre]} (marker remainder)]
        (if (and pre remainder)
          (recur )))
      output)))

(defn day9-2
  [in]
  (let [initial-input (if (string/index-of in "resources/") (slurp in) in)]
    (loop [input initial-input output nil]
      (if (seq input)
        (let [{:keys [remainder text]} (deep-process input)]
          (recur remainder (str output text)))
        output))))

;;; AoC 2018 Day 1 Part 1
(->> "resources/aoc2018/day1"
    slurp
    string/split-lines
    (transduce (comp (map clojure.edn/read-string)) +))

;;; AoC 2018 Day 1 Part 2
(defn make-infinite-scroll
  [original]
  (letfn [(infinite-scroll
            [[head & tail]]
             (lazy-seq (cons head (if (empty? tail)
                                    (infinite-scroll original)
                                    (infinite-scroll tail)))))]
    (infinite-scroll original)))

(defn aoc2018-day1-2
  ([] (->> "resources/aoc2018/day1"
           slurp
           string/split-lines
           (map clojure.edn/read-string)
           aoc2018-day1-2))
  ([freqs] (loop [prev-freqs #{0}
                  freq-changes (make-infinite-scroll freqs)
                  last-freq 0]
             (let [current-freq (+ (first freq-changes) last-freq)]
               (if (some #{current-freq} prev-freqs)
                 current-freq
                 (recur (conj prev-freqs current-freq)
                        (next freq-changes)
                        current-freq))))))

(aoc2018-day1-2 '(+1, -2, +3, +1)) ;; => 2
(aoc2018-day1-2 '(+1, -1)) ;; => 0
(aoc2018-day1-2 '(+3, +3, +4, -2, -4)) ;; => 10
(aoc2018-day1-2 '(-6, +3, +8, +5, -6)) ;; => 5
(aoc2018-day1-2 '(+7, +7, -2, -7, -4)) ;; => 14
(aoc2018-day1-2) ;; It doesn't finish (at least before losing my patience)

(defn changes-from-start
  "Return the changes from the start instead of from the previous change."
  [changes]
  (reduce
   (fn [res curr]
     (conj res ((fnil + 0) (last res) curr)))
   []
   changes))

(let [from-start (->> "resources/aoc2018/day1"
                      slurp
                      string/split-lines
                      (map clojure.edn/read-string)
                      changes-from-start)]
  (prn (apply min from-start)) ;; => -125057
  (prn (apply max from-start))) ;; => 662

;; Start is 0, 454, 908, ..

(->> "resources/aoc2018/day1"
     slurp
     string/split-lines
     (map clojure.edn/read-string)
     count)
;; => 957

(let [from-start (->> "resources/aoc2018/day1"
                      slurp
                      string/split-lines
                      (map clojure.edn/read-string)
                      changes-from-start)]
  {:s0 from-start :s1 (mapv (partial + 454) from-start)})

(->> "resources/aoc2018/day1"
     slurp
     string/split-lines
     (map clojure.edn/read-string)
     changes-from-start
     sort)

(float (/ 125057 454)) ;; => 275.45593

(defn aoc2018-day1-2-2
  ([] (->> "resources/aoc2018/day1"
           slurp
           string/split-lines
           (map clojure.edn/read-string)
           aoc2018-day1-2-2))
  ([freqs] (let [jump (reduce + freqs)]
             (loop [freq-changes-from-start (changes-from-start freqs)
                   prev-freqs #{0}]
               (if (some (set freq-changes-from-start) prev-freqs)
                 (first (filter prev-freqs freq-changes-from-start))
                 (recur
                  (map (partial + jump) freq-changes-from-start)
                  (into prev-freqs freq-changes-from-start)))))))

(time (aoc2018-day1-2-2 '(+1, -2, +3, +1)))
;; => 2;; => "Elapsed time: 0.152813 msecs"
(time (aoc2018-day1-2-2 '(+1, -1)))
;; => 0;; => "Elapsed time: 0.115099 msecs"
(time (aoc2018-day1-2-2 '(+3, +3, +4, -2, -4)))
;; => 10;; => "Elapsed time: 0.140591 msecs"
(time (aoc2018-day1-2-2 '(-6, +3, +8, +5, -6)))
;; => 5;; => "Elapsed time: 0.165174 msecs"
(time (aoc2018-day1-2-2 '(+7, +7, -2, -7, -4)))
;; => 14;; => "Elapsed time: 0.153162 msecs"
(time (aoc2018-day1-2-2))
;; => 566;; => "Elapsed time: 6282.90518 msecs"

(defn aoc2018-day1-2-3
  ([] (->> "resources/aoc2018/day1"
           slurp
           string/split-lines
           (map clojure.edn/read-string)
           aoc2018-day1-2-3))
  ([freqs] (let [jump (reduce + freqs)]
             (loop [freq-changes-from-start (changes-from-start freqs)
                   prev-freqs #{0}]
               (if (some (set freq-changes-from-start) prev-freqs)
                 (first (sequence (comp (filter prev-freqs)
                                        (take 1))
                                  freq-changes-from-start))
                 (recur
                  (map (partial + jump) freq-changes-from-start)
                  (into prev-freqs freq-changes-from-start)))))))

(time (aoc2018-day1-2-3 '(+1, -2, +3, +1)))
;; => 2;; => "Elapsed time: 0.256597 msecs"
(time (aoc2018-day1-2-3 '(+1, -1)))
;; => 0;; => "Elapsed time: 0.168387 msecs"
(time (aoc2018-day1-2-3 '(+3, +3, +4, -2, -4)))
;; => 10;; => "Elapsed time: 0.180609 msecs"
(time (aoc2018-day1-2-3 '(-6, +3, +8, +5, -6)))
;; => 5;; => "Elapsed time: 0.191505 msecs"
(time (aoc2018-day1-2-3 '(+7, +7, -2, -7, -4)))
;; => 14;; => "Elapsed time: 0.195416 msecs"
(time (aoc2018-day1-2-3))
;; => 566;; => "Elapsed time: 6174.851763 msecs"

;;; AoC 2018 Day 2 Part 1
(let [ids (->> "resources/aoc2018/day2"
               slurp
               string/split-lines
               (map frequencies))
      twos (filter #(some #{2} (vals %)) ids)
      threes (filter #(some #{3} (vals %)) ids)]
  (* (count twos) (count threes)))
;; => 7163

;;; AoC 2018 Day 2 Part 2

(defn distance
  [string-a string-b]
  (apply + (map (fn [char-a char-b]
                  (if (= char-a char-b)
                    0
                    1)) string-a string-b)))

(defn different-letters
  [[string-a string-b]]
  (->> (map (fn [char-a char-b] (when (= char-a char-b) char-a)) string-a string-b)
       (remove nil?)
       (apply str)))

(->> "resources/aoc2018/day2"
     slurp
     string/split-lines
     sort
     (partition 2 1)
     (filter (fn [[a b]] (= 1 (distance a b))))
     first
     different-letters)
;; => ighfbyijnoumxjlxevacpwqtr

;;; Aoc 2018 Day 3 Part 1
(defn create-area
  "Create an area.
  An `area` is represented by a collection of `space`s.
  A `space` is represented by a vector of [column row].
  Rows and columns go from 0 .. n-1."
  [[start-column start-row] width height]
  (for [x (range start-column (+ start-column width))
        y (range start-row (+ start-row height))]
    [x y]))

(defn aoc2018-day3-part1
  []
  (->> "resources/aoc2018/day3"
       slurp
       string/split-lines
       (transduce
        (comp (map #(re-find #"@ (\d+),(\d+): (\d+)x(\d+)" %))
              (map (fn [[_ x y w h]] [[(clojure.edn/read-string x) (clojure.edn/read-string y)] (clojure.edn/read-string w) (clojure.edn/read-string h)]))
              (map #(apply create-area %)))
        into
        '())
       frequencies
       (filter (fn [[space freq]] (<= 2 freq)))
       keys
       count))

(aoc2018-day3-part1)
;; => 109716

;;; AoC 2018 Day 3 Part 2
(defn create-area-part2
  "Create an area.
  An `area` is represented by a collection of `space`s.
  A `space` is represented by a vector of [column row].
  Rows and columns go from 0 .. n-1."
  [id [start-column start-row] width height]
  (for [x (range start-column (+ start-column width))
        y (range start-row (+ start-row height))]
    {:id id :space [x y]}))

(defn aoc2018-day3-part2
  []
  (let [spaces-id (->> "resources/aoc2018/day3"
                       slurp
                       string/split-lines
                       (transduce
                        (comp (map #(re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" %))
                              (map (fn [[_ id x y w h]] [(clojure.edn/read-string id) [(clojure.edn/read-string x) (clojure.edn/read-string y)] (clojure.edn/read-string w) (clojure.edn/read-string h)]))
                              (map #(apply create-area-part2 %)))
                        into
                        '()))
        ids (->> spaces-id
                 (map :id)
                 set)
        overlapping (->> spaces-id
                         (map :space)
                         frequencies
                         (filter (fn [[space freq]] (<= 2 freq)))
                         keys)
        overlapping-ids (->> spaces-id
                             (filter (fn [{:keys [space]}] (some #{space} overlapping)))
                             (map :id)
                             set)]
    (clojure.set/difference ids overlapping-ids)))
(aoc2018-day3-part2) ;; => Never finishes

(defn no-overlapping-ids
  [overlapping spaces-id]
  (let [overlapping (set overlapping)]
    (loop [result (->> spaces-id
                      (map :id)
                      set)
          spaces-id-to-double-check spaces-id]
     (if (seq spaces-id-to-double-check)
       (let [{:keys [id space]} (first spaces-id-to-double-check)]
         (if (overlapping space)
           (recur (disj result id) (remove #(= id (:id %)) (next spaces-id-to-double-check)))
           (recur result (next spaces-id-to-double-check))))
       result))))

(defn aoc2018-day3-part2-2
  []
  (let [spaces-id (->> "resources/aoc2018/day3"
                       slurp
                       string/split-lines
                       (transduce
                        (comp (map #(re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" %))
                              (map (fn [[_ id x y w h]] [(clojure.edn/read-string id) [(clojure.edn/read-string x) (clojure.edn/read-string y)] (clojure.edn/read-string w) (clojure.edn/read-string h)]))
                              (map #(apply create-area-part2 %)))
                        into
                        '()))
        overlapping (->> spaces-id
                         (map :space)
                         frequencies
                         (filter (fn [[space freq]] (<= 2 freq)))
                         keys)]
    (time (no-overlapping-ids overlapping spaces-id))))

(aoc2018-day3-part2-2)
;; => #{124};; => "Elapsed time: 143092.900008 msecs"

;;; AoC 2018 Day 4 Part 1
(defn parse-minutes
  [minute]
  (if-let [[_ minute-one-digit] (re-find #"0(\d)" minute)]
    (clojure.edn/read-string minute-one-digit)
    (clojure.edn/read-string minute)))

(defn aoc2018-day4-part1
  []
  (->> "resources/aoc2018/day4"
       slurp
       string/split-lines
       (sequence (comp
                  (map #(re-find #"\[(\d\d\d\d-\d\d-\d\d) (\d\d):(\d\d)\] (.+)" %))
                  (map (fn [[_ date hour minute action]]
                         {:date-and-time (str date " " hour ":" minute)
                          :minute (parse-minutes minute)
                          :action (string/trim action)}))))
       (sort-by :date-and-time)))

(aoc2018-day4-part1)
