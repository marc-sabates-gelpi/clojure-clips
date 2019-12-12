(ns clips.aoc.2019.day3
  (:require [clojure.repl :refer :all]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [taoensso.timbre :refer [debug spy]]))

(defn- abs
  [x]
  (if (neg? x)
    (- x)
    x))

(defn- d
  [a b]
  (+ (abs (- (:x a) (:x b)))
     (abs (- (:y a) (:y b)))))

(defn- dist-from-origin
  [{:keys [a b] :as _segment}]
  (min (d {:x 0 :y 0} a) (d {:x 0 :y 0} b)))

(defn- add-distance
  [segment]
  (assoc segment :dist (dist-from-origin segment)))

(defn- magnitude
  [s]
  (->> s (re-find #"[0-9]+") edn/read-string))

(defmulti make-directed-segment (fn [_ s] (-> s first str)))

(defmethod make-directed-segment "U"
  [{:keys [b]} s]
  {:a   b
   :dir :u
   :b   (update b :y + (magnitude s))})

(defmethod make-directed-segment "D"
  [{:keys [b]} s]
  {:a   b
   :dir :d
   :b   (update b :y - (magnitude s))})

(defmethod make-directed-segment "L"
  [{:keys [b]} s]
  {:a   b
   :dir :l
   :b   (update b :x - (magnitude s))})

(defmethod make-directed-segment "R"
  [{:keys [b]} s]
  {:a   b
   :dir :r
   :b   (update b :x + (magnitude s))})

(defn make-segment
  [acc el]
  (conj acc (-> (last acc)
                (or {:b {:x 0 :y 0}})
                (make-directed-segment el)
                add-distance)))

(defn enhance
  "Return collection of segments (map)."
  [s]
  (reduce make-segment [] (string/split s #",")))

(comment (sort-by :dist (enhance "R1009,D335,L942,D733,L398,U204")))

(def vertical-dir? #{:u :d})
(def horizontal-dir? #{:l :r})
(def vertical-segment? (comp vertical-dir? :dir))
(def horizontal-segment? (comp horizontal-dir? :dir))

(defn perpendicular?
  [{dir-a :dir :as _a} {dir-b :dir :as _b}]
  (or
    (and (vertical-dir? dir-a) (horizontal-dir? dir-b))
    (and (horizontal-dir? dir-a) (vertical-dir? dir-b))
    false))

(defn reverse-dir
  [dir]
  (case dir
    :u :d
    :d :u
    :l :r
    :r :l))

(defn normalise-dir
  "Return a segment with the vertices ordered by nearer from 0 on the axes ascendant.
  It updates properly the direction."
  [{{ax :x ay :y :as a} :a {bx :x by :y :as b} :b :as segment}]
  (if (and (<= ax bx) (<= ay by))
    segment
    (-> segment
        (assoc :a b)
        (assoc :b a)
        (update :dir reverse-dir))))

(defn intersect?
  "Return the point where 2 segments intersect; `nil` otherwise.
  Assumptions: 1) Segments run vertically or horizontally only.
               2) Intersections happen on perpendicular segments only."
  [s1 s2]
  (let [{{s1ax :x s1ay :y} :a {s1bx :x s1by :y} :b :as s1} (normalise-dir s1)
        {{s2ax :x s2ay :y} :a {s2bx :x s2by :y} :b :as s2} (normalise-dir s2)]
    (and
      (perpendicular? s1 s2)
      (or
        (and
          (<= s1ay s2ay s2by s1by)
          (<= s2ax s1ax s1bx s2bx))
        (and
          (<= s2ay s1ay s1by s2by)
          (<= s1ax s2ax s2bx s1bx))))))

(defn intersection
  "Return the intersection point.
  Preconditions: 1) s1 and s2 intersect.
                 2) s1 and s2 are perpendicular."
  [s1 s2]
  (let [{{s1ax :x s1ay :y} :a :as s1} (normalise-dir s1)
        {{s2ax :x s2ay :y} :a :as _s2} (normalise-dir s2)]
    (if (horizontal-segment? s1)
      {:x s2ax :y s1ay}
      {:x s1ax :y s2ay})))

(deftest unit-testing
  (testing "Intersecting right + up segment. Intersects at {:x 4 :y 2} (mid segment)."
    (let [s1 {:dir :r :a {:x 2 :y 2} :b {:x 6 :y 2}}
          s2 {:dir :u :a {:x 4 :y 1} :b {:x 4 :y 5}}]
      (is (true? (intersect? s1 s2)))
      (is (= {:x 4 :y 2} (intersection s1 s2)))))
  (testing "Intersecting left + down segment. Intersects at {:x 4 :y 2} (mid segment)."
    (let [s1 {:dir :l :b {:x 2 :y 2} :a {:x 6 :y 2}}
          s2 {:dir :d :b {:x 4 :y 1} :a {:x 4 :y 5}}]
      (is (true? (intersect? s1 s2)))
      (is (= {:x 4 :y 2} (intersection s1 s2)))))
  (testing "Intersecting left + down segment. Intersects at {:x 4 :y 2} (boundary)."
    (let [s1 {:dir :l :b {:x 2 :y 2} :a {:x 6 :y 2}}
          s2 {:dir :d :b {:x 4 :y 2} :a {:x 4 :y 5}}]
      (is (true? (intersect? s1 s2)))
      (is (= {:x 4 :y 2} (intersection s1 s2)))))
  (testing "Intersecting up + right segment. Intersects at {:x 5 :y 3} (boundary)."
    (let [s1 {:dir :u :a {:x 5 :y 2} :b {:x 5 :y 10}}
          s2 {:dir :r :a {:x 4 :y 3} :b {:x 10 :y 3}}]
      (is (true? (intersect? s1 s2)))
      (is (= {:x 5 :y 3} (intersection s1 s2)))))
  (testing "Non-intersecting left + down segment."
    (is (false? (intersect? {:dir :l :b {:x 2 :y 2} :a {:x 6 :y 2}} {:dir :d :b {:x 4 :y 3} :a {:x 4 :y 5}}))))
  (testing "Non-intersecting up + right segment."
    (is (false? (intersect? {:dir :u :a {:x 1 :y 2} :b {:x 1 :y 10}} {:dir :r :a {:x 4 :y 1} :b {:x 10 :y 1}})))))

(comment
  (clojure.test/run-tests *ns*)
  )
