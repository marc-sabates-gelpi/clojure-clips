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

;;; Session 27/05/2018

;; `some-in` could follow the HOF approach
;; using a `pred` instead of `k` `v`.

(defmacro some-full-map [pred coll]
  "`some` in `coll` returning the full map.
  `pred` provides selection."
  `(some
    (fn [m#]
      (when (~pred m#)
        m#))
    ~coll))

;; refactor

(defmacro some-in [k v coll]
  "`some` val `v` in `coll` of maps with key `k`."
  `(some
    (fn [{:keys [~k] :as whole#}]
      (when (#{~v} ~(symbol (.getName k)))
        whole#))
    ~coll))

(let [name-looked-for :name2
      responses '({:application.response/greenhouse-name :name0 :application.response/answer "Answer 0"} {:application.response/greenhouse-name :name1 :application.response/answer "Answer 1"} {:application.response/greenhouse-name :name2 :application.response/answer "Answer 2"})]
  [;; original
   (->> responses
         (filter #(= name-looked-for (:application.response/greenhouse-name %)))
         first
         :application.response/answer)

   ;; alternative using `some`
   (->> responses
        (some (fn [{:keys [application.response/greenhouse-name] :as whole}]
                (when (some #{name-looked-for} (list greenhouse-name))
                  whole)))
        :application.response/answer)

   ;; alternative using custom `some-in` macro
   (->> responses
        (some-in application.response/greenhouse-name name-looked-for)
        :application.response/answer)

   ;; alternative using custom `some-full-map` macro
   (->> responses
        (some-full-map (comp
                        #{name-looked-for}
                        :application.response/greenhouse-name))
        :application.response/answer)

   ;; alternative using `#{}`
   (->> responses
        (filter (comp
                 #{name-looked-for}
                 :application.response/greenhouse-name))
        first
        :application.response/answer)])

;;;; Refactor `update-answer`

;;; Original

(defn update-answer
  "Updates the application in the db with the given question/answer, or adds it if it doesn't exist"
  [responses greenhouse-name answer]
  (if-let [idx (->> responses
                    (keep-indexed (fn-traced [idx response]
                                    (when (= greenhouse-name (:application.response/greenhouse-name response))
                                      idx)))
                    first)]
    (update responses idx assoc :application.response/answer answer)
    (conj responses {:application.response/greenhouse-name greenhouse-name
                     :application.response/answer answer})))

;;; Attempt 1
;;; Failure; It doesnt' work..

(defn update-answer
  "Updates the `greenhouse-name` question in `responses` with the given `answer`, or adds it."
  [responses greenhouse-name answer]
  (vec (or ; FIXME: It always returns the `(map)` execution
        (map
         (fn [m]
           (if ((comp #{greenhouse-name} :application.response/greenhouse-name) m)
             (update m assoc :application.response/answer answer)
             m))
         responses)
        (conj responses {:application.response/greenhouse-name greenhouse-name
                         :application.response/answer answer})))
  #_(if-let [idx (->> responses
                    (keep-indexed (fn-traced [idx response]
                                    (when (= greenhouse-name (:application.response/greenhouse-name response))
                                      idx)))
                    first)]
    (update responses idx assoc :application.response/answer answer)
    (conj responses {:application.response/greenhouse-name greenhouse-name
                     :application.response/answer answer})))

;;;; 28/05/2018

(realized? '(1 2 3)) ; ClassCastException clojure.lang.PersistentList cannot be cast to clojure.lang.IPending

(realized? (seq '(1 2 3))) ; idem

(realized? (iterate inc 0)) ; it doesn't tell if it has been fully realized

;;; Attempt 2

(reduce-kv #(conj %1 {%2 %3}) [] [:a :b :c])

(some)

;; macro definition
(defmacro some-indexed [pred coll]
  "Return index of the first `pred` true."
  `(letfn [(some-ix# [pred# ix# coll#]
             (when (seq coll#)
               (or (and (pred# (first coll#)) ix#)
                   (recur pred# (inc ix#) (next coll#)))))]
     (some-ix# ~pred 0 ~coll)))

(some-indexed #{2} [0 1 2 3 4 5])

(some-indexed (comp #{2} :nspace/field1) [{:nspace/field1 0 :nspace/field2 :v} {:nspace/field1 1 :nspace/field2 :v} {:nspace/field1 2 :nspace/field2 :v} {:nspace/field1 3 :nspace/field2 :v}])

(some-indexed (comp #{-1} :nspace/field1) [{:nspace/field1 0 :nspace/field2 :v} {:nspace/field1 1 :nspace/field2 :v} {:nspace/field1 2 :nspace/field2 :v} {:nspace/field1 3 :nspace/field2 :v}])

;; function definition
(defn update-answer
  "Updates the `greenhouse-name` question in `responses` with the given `answer`, or adds it."
  [responses greenhouse-name answer]
  (if-let [idx (some-indexed (comp #{greenhouse-name} :application.response/greenhouse-name) responses)]
    (update responses idx assoc :application.response/answer answer)
    (conj responses {:application.response/greenhouse-name greenhouse-name
                     :application.response/answer answer})))

;; test
(let [name-looked-for :name2
      responses [{:application.response/greenhouse-name :name0 :application.response/answer "Answer 0"} {:application.response/greenhouse-name :name1 :application.response/answer "Answer 1"} {:application.response/greenhouse-name :name2 :application.response/answer "Answer 2"}]]
  (clojure.pprint/pprint (update-answer responses name-looked-for "NEW ANSWER!"))
  (clojure.pprint/pprint (update-answer responses :non-existent-name "NEW ANSWER!")))

;;; spec optional vs nil
(require '[clojure.spec.alpha :as s])

(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(s/def ::email-type (s/and string? #(re-matches email-regex %)))
(s/def ::first-name string?)
(s/def ::last-name string?)
(s/def ::email ::email-type)
(s/def ::phone string?)
(s/def ::person (s/keys :req-un [::first-name ::last-name ::email]
                        :opt-un [::phone]))
(s/valid? ::person
  {:first-name "Elon"
   :last-name "Musk"
   :email "elon@example.com"})

(s/valid? ::person
  {:first-name "Elon"
   :last-name "Musk"
   :email "elon@example.com"
   :phone nil})

(s/explain ::person
  {:first-name "Elon"
   :last-name "Musk"
   :email "elon@example.com"
   :phone nil})

(s/def ::phone (s/nilable string?))
(s/valid? ::person
  {:first-name "Elon"
   :last-name "Musk"
   :email "elon@example.com"
   :phone nil})

;;; (s/def :ring.request/query-string
;;;  (s/with-gen string? gen-query-string))
;;; is NOT nilable, and somehow we are getting
;;; :query-string nil,
;;;               ^^^
;;; damn it !


;;; Session 13/06/2018

(defn remote-call
  "Fake remote call."
  [page]
  (printf "Remote call for page %d" page)
  (when (>= 2 page)
    (range (* page 10) (* (inc page) 10))))

(defn auth0-users
  "Lazy seq of auth0 users."
  [coll page]

  (if (seq coll)
    (lazy-seq (cons (first coll) (auth0-users (rest coll) page)))
    (when-let [response (seq (clips.core/remote-call page))]
      (lazy-seq (cons (first response) (auth0-users (rest response) (inc page)))))))


(defn make-auth0-users
  "Make auth0 users lazy list."
  []
  (auth0-users nil 0))

(def users (make-auth0-users))

(take 40 users)
