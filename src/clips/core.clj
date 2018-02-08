(ns clips.core
  (:require [clojure.core.async :as async]))
;;31/01/2018
(def ^:constant VERT_PIPE \|)
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
(defn get-straight-pipe [pipes start-point get-next]
  (loop [next (get-next start-point) collected []]
    (let [pipe-bit (get-in pipes next \space)]
      (if (and
           (or
            (= pipe-bit TURN_PIPE)
            (letter? pipe-bit))
           (= \space (get-in pipes (get-next next) \space)))
        {:end-pos next :collected (conj collected pipe-bit)}
        (recur (get-next next) (conj collected pipe-bit))))))
(defn move [[row-dir col-dir] [row col]]
  [(+ row-dir row) (+ col-dir col)])
(get-straight-pipe [[\| \space \space] [\| \space \space] [\+ \| \space]] [0 0] (partial move DOWN))
(defn abs [n]
  (if (<= 0 n)
    n
    (* -1 n)))
(defn letter? [c]
  (let [n (int c)]
    (or (<= 97 n 122) (<= 65 n 90))))
(defn follow-pipe [pipes]
  (loop [dir DOWN current-pos [-1 (find-pipes-beginning pipes)] result #:result{:letters [] :steps [] :double-accountancy [] :raw-pipe []}]
    (if (= dir :end)
      result
      (let [{:keys [end-pos collected]} (get-straight-pipe pipes current-pos (partial move dir))]
        (recur
         (find-next-direction pipes end-pos dir)
         end-pos
         (-> result
             (update :result/letters into (comp (filter letter?)) collected)
             (update :result/steps conj (count collected))
             (update :result/double-accountancy conj (max
                                                      (abs
                                                       (- (get current-pos 0) (get end-pos 0)))
                                                      (abs
                                                       (- (get current-pos 1) (get end-pos 1)))))
             (update :result/raw-pipe conj collected)))))))
(follow-pipe [[\| \space \F] [\| \space \|] [\+ \- \+]])
;;01/02/2018
(-> "tubes"
 slurp
;; "     |          
;;  +-+ |          
;;  | | |  +--+    
;;  | | A  |  C    
;;  +---|--|-E---+ 
;;    F |  |  |  D 
;;      +B-+  +--+ 
;;                  "
;; "           |   
;;  +-+ +-+   A   
;;  | | | |   |   
;;  | | | | B-|-+ 
;;  | | | |   | | 
;;  | +-+ +---+ | 
;;  |           | 
;;  +-----------+ 
;;                "
    clojure.string/split-lines
    (as-> rows (sequence (comp
                          (map sequence)
                          (map vec))
                         rows))
    vec
    follow-pipe
    (update :result/letters #(reduce str %))
    (update :result/steps #(reduce + %))
    (update :result/double-accountancy #(reduce + %))
    (assoc :result/raw-pipe [])
    prn
    ;;(as-> results (spit "tubes-results" results))
    )
;; -- Part 2
(defn count-chars []
  (-> "tubes"
   slurp
;; "     |          
;;  +-+ |          
;;  | | |  +--+    
;;  | | A  |  C    
;;  +---|--|-E---+ 
;;    F |  |  |  D 
;;      +B-+  +--+ 
;;                 "
   (as-> input (clojure.string/replace input #"\s" "")
     (count input))))
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
   ;; "     |          
 ;; +-+ |          
 ;; | | |  +--+    
 ;; | | A  |  C    
 ;; +-|-|--|-E---+ 
 ;;   F |  |  |  D 
 ;;     +B-+  +--+ 
 ;;                "
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
;;02/02/2018
(defn print-pipes [pipes]
  (reduce str (map #(str (reduce str %) \newline) pipes)))
(defn get-straight-pipe-progress [initial-pipes [start-row start-col :as start-point] get-next]
  (loop [next (get-next start-point) collection [] pipes initial-pipes]
    (let [pipe-bit (get-in pipes next \space)]
      (if (or (= pipe-bit TURN_PIPE) (= pipe-bit \space))
        {:pos next :collection (conj collection pipe-bit) :pipes pipes}
        (recur (get-next next) (conj collection pipe-bit) (assoc-in pipes next \*))))))
(defn follow-pipe-progress [initial-pipes]
  (loop [dir DOWN current-pos [-1 (find-pipes-beginning initial-pipes)] collected-letters [] steps 0 current-pipes initial-pipes]
    (if (= dir :end)
      {:letters collected-letters :steps steps}
      (let [{:keys [pos collection pipes]} (get-straight-pipe-progress current-pipes current-pos (partial move dir))
            _ (do
                (spit "tubes-progress" (print-pipes pipes))
                (Thread/sleep 3000))]
        (recur
         (find-next-direction pipes pos dir)
         pos
         (into collected-letters (comp (filter letter?)) collection)
         (+ steps (count (remove #(= \space %) collection))) pipes)))))
(-> "tubes"
 slurp
;; " +-+ |          
;;  | | |  +--+    
;;  | | A  |  C    
;;  +---|--|-E---+ 
;;    F |  |  |  D 
;;      +B-+  +--+ "
    clojure.string/split-lines
    (as-> rows (sequence (comp
                          (map sequence)
                          (map vec))
                         rows))
    vec
    follow-pipe-progress
    (as-> result {:letters (reduce str (:letters result)) :steps (:steps result)})
    prn)
;;03/02/2018
;; --- Day 20: Particle Swarm --- Part 1
(-> "particles"
    slurp
    clojure.string/split-lines
    (as-> lines (sequence (comp
                           (map #(re-seq #"-?[0-9]+" %))
                           (map #(map read-string %))
                           (map vec)
                           (map (fn [[posx posy posz velx vely velz accx accy accz]] #:particle{:position {:x posx :y posy :z posz} :velocity {:x velx :y vely :z velz} :acceleration {:x accx :y accy :z accz}}))
                           (map-indexed #(assoc %2 :particle/id %1)))
                          lines))
    (as-> particles (sort-by (fn [{{:keys [x y z]} :particle/acceleration}]
                               (+ (abs x) (abs y) (abs z)))
                             particles))
    first
    ;;(as-> particles (find-closest [0 0 0] particles))
    prn)
(defn get-next-tick-state [initial-particles]
  (as-> initial-particles particles
    (pmap #(as-> % particle
             (update-in particle [:particle/velocity :x] + (get-in particle [:particle/acceleration :x]))
             (update-in particle [:particle/velocity :y] + (get-in particle [:particle/acceleration :y]))
             (update-in particle [:particle/velocity :z] + (get-in particle [:particle/acceleration :z]))
             (update-in particle [:particle/position :x] + (get-in particle [:particle/velocity :x]))
             (update-in particle [:particle/position :y] + (get-in particle [:particle/velocity :y]))
             (update-in particle [:particle/position :z] + (get-in particle [:particle/velocity :z])))
          particles)
    (remove-collisions particles)))
(defn remove-collisions [particles]
  (let [living-particles-ids (set
                              (map
                               :particle/id
                               (flatten
                                (filter
                                 #(= 1 (count %))
                                 (vals
                                  (group-by
                                   (fn [{{:keys [x y z]} :particle/position}] [x y z])
                                   particles))))))]
    (filter
     #(living-particles-ids (:particle/id %))
     particles)))
(defn continue?  [particles ticks]
  (>= ticks 1000))
(defn run-simulation [initial-particles]
  (loop [ticks 0 particles initial-particles]
    (if (continue? particles ticks)
      #:particles-simulation{:ticks ticks :particles particles}
      (recur (inc ticks) (get-next-tick-state particles)))))
;; -- Part 2
(-> "particles"
    slurp
    clojure.string/split-lines
    (as-> lines (sequence (comp
                           (map #(re-seq #"-?[0-9]+" %))
                           (map #(map read-string %))
                           (map vec)
                           (map (fn [[posx posy posz velx vely velz accx accy accz]] #:particle{:position {:x posx :y posy :z posz} :velocity {:x velx :y vely :z velz} :acceleration {:x accx :y accy :z accz}}))
                           (map-indexed #(assoc %2 :particle/id %1)))
                          lines))
    (as-> particles (run-simulation particles))
    (as-> particles (assoc particles :particles-simulation/count (count (:particles-simulation/particles particles))))
    (update :particles-simulation/particles empty)
    prn)
(filter #(#{0} (:particle/id %)) '({ :particle/position { :x -1021, :y -2406, :z 1428 }, :particle/velocity { :x 11, :y 24, :z -73 }, :particle/acceleration { :x 4, :y 9, :z 0 }, :particle/id 0 } { :particle/position { :x -1021, :y -2406, :z 1428 }, :particle/velocity { :x 11, :y 24, :z -73 }, :particle/acceleration { :x 4, :y 9, :z 0 }, :particle/id 1 }))
(filter #(#{0} (:id %)) '({:id 0} {:id 1}))
;; 06/02/2018
;; --- Day 21: Fractal Art --- Part 1
;; 07/02/2018
(require '[clojure.spec.alpha :as s])
(s/def ::axis #{:x :y})
(s/def ::rows-chars (s/coll-of (s/coll-of char?)))
(s/def ::rows-strings (s/coll-of string?))
(s/def ::rows (s/or :string ::rows-strings :char ::rows-chars))
;; (s/def ::rows (s/+ string?))
(s/def ::image (s/& (s/+ string?) #(= (count %) (count (first %)))))
(s/conform ::rows '(".#" "#."))
(s/conform ::rows '((\. \#) (\# \.)))
(defn make-image
  "Make image. Implementation of an image with an array of strings"
  [rows]
  {:post [(s/valid? ::image %)]}
  (let [[type value :as all] (s/conform ::rows rows)]
    (if (= all ::s/invalid)
      (throw (ex-info "Invalid input" (s/explain-data ::rows rows)))
      (if (= :string type)
        (into [] rows)
        (into [] (map (partial apply str) rows))))))
;; (defn make-image
;;   "Make image. Implementation of an image with an array of strings"
;;   [rows]
;;   {:pre [(s/valid? ::rows rows)]
;;    :post [(s/valid? ::image %)]}
;;   (into [] rows))
(defn get-rows
  "Get the rows of an image. Implementation of an image with an array of strings"
  [image]
  {:pre [(s/valid? ::image image)]
   :post [(s/valid? ::rows %)]}
  image)
(make-image '(".#" "#."))
(make-image '((\. \#) (\# \.)))
(get-rows (make-image '(".#" "#.")))
(defn flip
  "Mirros the image olong the axis"
  [image axis]
  {:pre [(s/valid? ::axis axis) (s/valid? ::image image)]}
  (if (= axis :y)
    (make-image (reverse (get-rows image)))
    (make-image (map #(->> %
                          reverse) (get-rows image)))))
(flip '("##" "#.") :y)
(flip '("##" "#.") :x)
(get-in ["01" "23"] [0 1])
(s/def ::rotation #{:90 :180 :270})
(defn rotate
  "Rotates an image anticlock wise"
  [image rotation]
  {:pre [(s/valid? ::image image) (s/valid? ::rotation rotation)]
   :post [(s/valid? ::image %)]}
  (cond
    (= :90 rotation) (-> image
                         get-rows
                         (as-> r (apply map vector r))
                         make-image
                         (flip :y))  
    (= :180 rotation) (-> image
                          (flip :x)
                          (flip :y))
    (= :270 rotation) (-> image
                          (rotate :90)
                          (rotate :180))))
(rotate (make-image '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9))) :180)
(rotate (make-image '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9))) :90)
(rotate (make-image '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9))) :270)
(apply map vector '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9)))
;; (-> "artist-rules"
;;     slurp
;;     clojure.string/split-lines
;;     (sequence (comp
;;                ())))
