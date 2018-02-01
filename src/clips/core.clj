(ns clips.core
  (:require [clojure.core.async :as async]))
;;31/01/2018
(def ^:constant VERT_PIPE \|)
(def ^:constant HORIZ_PIPE \-)
(def ^:constant TURN_PIPE \+)
(def ^:constant RIGHT [0 1])
(def ^:constant LEFT [0 -1])
(def ^:constant DOWN [1 0])
(def ^:constant UP [-1 0])
(defn find-pipes-beginning [[starting-line & _]]
  (reduce-kv (fn [found-pos k v] (if (= v VERT_PIPE) k found-pos)) nil starting-line))
(find-pipes-beginning [[\space \space \space \| \space \space] [\space \space \space \| \space \space]])
(find-pipes-beginning [[\| \space \space]])
(find-pipes-beginning [[\space \space \space \|] [\space \space \space \| \space \space]])
(find-pipes-beginning [[\space \space \space \space \space \space] [\space \space \space \| \space \space]])
(defn find-next-direction
  ([pipes pos] (find-next-direction pipes pos UP))
  ([pipes [row col] prev-dir]
   (cond
     (and (not= \space (get-in pipes [row (inc col)] \space)) (not= prev-dir LEFT)) RIGHT 
     (and (not= \space (get-in pipes [row (dec col)] \space)) (not= prev-dir RIGHT)) LEFT
     (and (not= \space (get-in pipes [(inc row) col] \space)) (not= prev-dir UP)) DOWN
     (and (not= \space (get-in pipes [(dec row) col] \space)) (not= prev-dir DOWN)) UP
     :else :end)))
(find-next-direction [[\space \space \space] [\space \| \space] [\space \| \space]] [1 1])
(defn get-straight-pipe [pipes [start-row start-col :as start-point] get-next]
  (loop [next (get-next start-point) collection []]
    (let [pipe-bit (get-in pipes next \space)]
      (if (or (= pipe-bit TURN_PIPE) (= pipe-bit \space))
        {:pos next :collection collection}
        (recur (get-next next) (conj collection pipe-bit))))))
(defn move [[row-dir col-dir] [row col]]
  [(+ row-dir row) (+ col-dir col)])
(get-straight-pipe [[\| \space \space] [\| \space \space] [\+ \| \space]] [0 0] (partial move DOWN))
(defn letter? [c]
  (let [n (int c)]
    (or (<= 97 n 122) (<= 65 n 90))))
(defn follow-pipe [pipes]
  (loop [dir DOWN current-pos [0 (find-pipes-beginning pipes)] collected-letters [] steps 0]
    (if (= dir :end)
      {:letters collected-letters :steps (inc steps)} ;; I inc because somehow I ignore the very first pipe in the grid (row 0)
      (let [{:keys [pos collection]} (get-straight-pipe pipes current-pos (partial move dir))]
        (recur (find-next-direction pipes pos dir) pos (into collected-letters (comp (filter letter?)) collection) (+ 1 steps (count collection))))))) ;; steps plus count plus 1 because I ignore the turning point (at each straight pipe)
(follow-pipe [[\| \space \F] [\| \space \|] [\+ \- \+]])
;;01/02/2018
(-> "tubes"
 slurp
;;     "     |          
;;      |  +--+    
;;      A  |  C    
;;  F---|--|-E---+ 
;;      |  |  |  D 
;;      +B-+  +--+ 
;; "
    clojure.string/split-lines
    (as-> rows (sequence (comp
                          (map sequence)
                          (map vec))
                         rows))
    vec
    follow-pipe
    (as-> result {:letters (reduce str (:letters result)) :steps (:steps result)})
    prn)
;; -- Part 2
(defn count-chars []
  (-> "tubes"
   slurp
;;    "     |          
;;      |  +--+    
;;      A  |  C    
;;  F---|--|-E---+ 
;;      |  |  |  D 
;;      +B-+  +--+ 
;; "
   (as-> input (clojure.string/replace input #"\s" ""))
   count))
(defn crossing? [pipes [r c]]
  (let [u (get-in pipes [r (dec c)] \space)
        d (get-in pipes [r (inc c)] \space)
        l (get-in pipes [(dec r) c] \space)
        r (get-in pipes [(inc r) c] \space)]
    (and (not= \space u)
         (not= \space d)
         (not= \space l)
         (not= \space r))))
(defn num-crossings [pipes]
  (for [r (range (count pipes)) c (range (count (first pipes)))]
    (crossing? pipes [r c])))
(defn count-crossings []
  (-> "tubes"
   slurp
;;    "     |          
;;      |  +--+    
;;      A  |  C    
;;  F---|--|-E---+ 
;;      |  |  |  D 
;;      +B-+  +--+ 
;; "
   clojure.string/split-lines
   (as-> rows (sequence (comp
                         (map sequence)
                         (map vec))
                        rows))
   vec
   num-crossings
   frequencies
   (get true)))
(prn (+ (count-chars) (count-crossings)))
