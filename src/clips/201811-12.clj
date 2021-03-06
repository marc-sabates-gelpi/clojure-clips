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

(defn parse-action
  [{:keys [action] :as entry}]
  (condp re-find action
    #"wakes up" (assoc entry :action :wake-up)
    #"falls asleep" (assoc entry :action :sleep)
    #"Guard #(\d+) begins shift" :>> (fn [[_ id]]
                                       (-> entry
                                          (assoc :action :begin)
                                          (assoc :id (clojure.edn/read-string id))))
    #"stop" (assoc entry :action :stop)))

(defmulti state-machine
  "Assumption: There are no inconsistent states, i.e. shift begins -> wakes up."
  (fn [_ {:keys [action]}] action))

(defmethod state-machine :begin
  [{:keys [current] :as state} {:keys [id]}]
  (let [closed (some-> current
                       (update (get-in current [:prev :type]) conj (range (get-in current [:prev :time]) 60))
                       (dissoc :prev))
        new-state (assoc state :current {:prev {:time 0
                                                :type :wake-up}
                                         :id id})]
    (if closed
      (update new-state :history conj closed)
      new-state)))

(defmethod state-machine :sleep
  [{:keys [current] :as state} {:keys [minute]}]
  (let [updated-current (-> current
                            (update :wake-up conj (range (get-in current [:prev :time]) minute))
                            (assoc :prev {:time minute
                                          :type :sleep}))]
    (assoc state :current updated-current)))

(defmethod state-machine :wake-up
  [{:keys [current] :as state} {:keys [minute]}]
  (let [updated-current (-> current
                            (update :sleep conj (range (get-in current [:prev :time]) minute))
                            (assoc :prev {:time minute
                                          :type :wake-up}))]
    (assoc state :current updated-current)))

(defmethod state-machine :stop
  [{:keys [current history] :as state} _]
  (let [closed (some-> current
                       (update (get-in current [:prev :type]) conj (range (get-in current [:prev :time]) 60))
                       (dissoc :prev))]
    (if closed
      (conj history closed)
      history)))

(defn find-uber-sleeper
  [figures]
  (->> figures
       (map (fn [[k v]]
              [k (reduce
                  (fn [total {:keys [sleep]}]
                    (apply + total (map count sleep)))
                  0
                  v)]))
       (sort-by second)
       last
       first))

(defn aoc2018-day4-part1
  ([]
   (-> "resources/aoc2018/day4"
       slurp
       aoc2018-day4-part1))
  ([text]
   (let [guard-figures (->> text
                            string/split-lines
                            (sequence (comp
                                       (map #(re-find #"\[(\d\d\d\d-\d\d-\d\d) (\d\d):(\d\d)\] (.+)" %))
                                       (map (fn [[_ date hour minute action]]
                                              {:date-and-time (str date " " hour ":" minute)
                                               :minute (parse-minutes minute)
                                               :action (string/trim action)}))))
                            (#(conj % {:date-and-time "9999-12-31 23:59"
                                       :action "stop"}))
                            (sort-by :date-and-time)
                            (transduce (map parse-action) (completing state-machine) {})
                            (group-by :id))
         uber-sleeper (find-uber-sleeper guard-figures)]
     (->> (get guard-figures uber-sleeper)
          (mapcat :sleep)
          (reduce into)
          frequencies
          (sort-by val)
          last
          key
          (* uber-sleeper))))) 

(-> "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"
    aoc2018-day4-part1)
;; => 240

(aoc2018-day4-part1);; => 103720

;;; AoC 2018 Day 4 Part 2

(defn aoc2018-day4-part2
  ([]
   (-> "resources/aoc2018/day4"
       slurp
       aoc2018-day4-part2))
  ([text]
   (->> text
        string/split-lines
        (sequence (comp
                   (map #(re-find #"\[(\d\d\d\d-\d\d-\d\d) (\d\d):(\d\d)\] (.+)" %))
                   (map (fn [[_ date hour minute action]]
                          {:date-and-time (str date " " hour ":" minute)
                           :minute (parse-minutes minute)
                           :action (string/trim action)}))))
        (#(conj % {:date-and-time "9999-12-31 23:59"
                   :action "stop"}))
        (sort-by :date-and-time)
        (transduce (map parse-action) (completing state-machine) {})
        (group-by :id)
        (sequence (comp
                   (map (fn [[k v]]
                          {:id k :minutes (reduce into '() (mapcat (comp concat :sleep) v))}))
                   (map #(update % :minutes frequencies))
                   (map #(update % :minutes (partial map (fn [[k v]] [k v]))))
                   (map #(update % :minutes (partial sort-by second)))
                   (map #(update % :minutes last))))
        (sort-by (fn [{[_ freq] :minutes}]
                   freq))
        last
        ((fn [{[m _] :minutes :keys [id]}]
           (* id m))))))

(-> "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"
    aoc2018-day4-part2)
;; => 4455

(aoc2018-day4-part2)
;; => 110913

;;; AoC 2018 Day 5 Part 1
(slurp "resources/aoc2018/day5")

(with-open [r (clojure.java.io/reader "resources/aoc2018/day5")]
  (let [file (line-seq r)]
    (run! prn file)))

(defn abs
  [n]
  (if (> 0 n)
    (* -1 n)
    n))

(defn react?
  [a b]
  (and (not (nil? a)) (= 32 (abs (- (int a) (int b))))))

(defn react
  [resulting-coll current-unit]
  (let [prev-unit (last resulting-coll)]
    (if (react? prev-unit current-unit)
      (vec (butlast resulting-coll))
      (conj resulting-coll current-unit))))

(defn aoc2018-day5-part1
  ([] (-> "resources/aoc2018/day5"
          slurp
          aoc2018-day5-part1))
  ([coll]
   (count (reduce (fnil react []) nil coll))))

(aoc2018-day5-part1 "dabAcCaCBAcCcaDA")
;; => 10

(time (aoc2018-day5-part1))
;; => 9238;; => "Elapsed time: 41315.801065 msecs"

;;; AoC2018 Day 5 Part 2
(defn test-unit
  [coll unit]
  (->> coll
       (remove #(= (first (string/lower-case %)) unit) )
       aoc2018-day5-part1))

(defn aoc2018-day5-part2
  ([] (-> "resources/aoc2018/day5"
          slurp
          aoc2018-day5-part2))
  ([coll]
   (->> (map char (range (int \a) (inc (int \z))))
        (pmap (partial test-unit coll))
        sort
        first)))

(time (aoc2018-day5-part2 "dabAcCaCBAcCcaDA"))
;; => 4;; => "Elapsed time: 8.282688 msecs"

(time (aoc2018-day5-part2))
;; => 4052;; => "Elapsed time: 611012.228307 msecs"

;;; AoC 2018 Day 6 Part 1

