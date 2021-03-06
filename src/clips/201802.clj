(ns clips.core
  (:require [clojure.core.async :as async]
            [spyscope.core]))
;;31/01/2018
(def ^:const VERT_PIPE \|)
(def ^:const TURN_PIPE \+)
(def ^:const RIGHT [0 1])
(def ^:const LEFT [0 -1])
(def ^:const DOWN [1 0])
(def ^:const UP [-1 0])
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
(s/def ::image (s/& (s/+ string?) (fn [coll] (let [num-rows (count coll)] (= num-rows (get (frequencies (map count coll)) num-rows))))))
(s/conform ::rows '(".#" "#."))
(s/conform ::rows '((\. \#) (\# \.)))
(defn make-image
  "Make image. Implementation of an image with an array of strings"
  [rows]
  {:post [(s/valid? ::image %)]}
  (let [[type value :as all] (s/conform ::rows rows)]
    (if (= all ::s/invalid)
      (throw (ex-info "Invalid input" (s/explain-data ::rows rows)))
      (cond
        (= :string type) (into [] rows)
        (= :char type) (into [] (map (partial apply str) rows))))))
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
  [axis image]
  {:pre [(s/valid? ::axis axis) (s/valid? ::image image)]}
  (if (= axis :y)
    (make-image (reverse (get-rows image)))
    (make-image (map #(->> %
                          reverse) (get-rows image)))))
(flip :y '("##" "#."))
(flip :x '("##" "#."))
(get-in ["01" "23"] [0 1])
(s/def ::rotation #{:90 :180 :270})
(defn rotate
  "Rotates an image anticlock-wise"
  [rotation image]
  {:pre [(s/valid? ::image image) (s/valid? ::rotation rotation)]
   :post [(s/valid? ::image %)]}
  (cond
    (= :90 rotation) (-> image
                         get-rows
                         (as-> r (apply map vector r))
                         make-image
                         (as-> i (flip :y i)))  
    (= :180 rotation) (as-> image i
                          (flip :x i)
                          (flip :y i))
    (= :270 rotation) (as-> image i
                          (rotate :90 i)
                          (rotate :180 i))))
(rotate :180 (make-image '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9))))
(rotate :90 (make-image '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9))))
(rotate :270 (make-image '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9))))
(apply map vector '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9)))
(= (make-image '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9))) (rotate :180 (rotate :180 (make-image '((\1 \2 \3) (\4 \5 \6) (\7 \8 \9))))))
(def fx (partial flip :x))
(def fy (partial flip :y))
(def rot90 (partial rotate :90))
(defn test-all-moves [image rule]
  {:pre [(s/valid? ::image image) (s/valid? (s/and string? (s/or :two ::rule-two :three ::rule-three)) rule)]
   :post [(s/valid? (s/nilable ::image) %)]}
  (let [rows (-> rule
                 (clojure.string/replace #" " "")
                 (as-> r (re-seq #"[\.#]+" r)))
        [orig dest] (if (= 5 (count rows))
                      [(make-image (take 2 rows)) (make-image (drop 2 rows))]
                      [(make-image (take 3 rows)) (make-image (drop 3 rows))])]
    (loop [current image ops [fx fy fx rot90 fx fy fx]]
      (cond
        (= current orig) (make-image dest)
        (empty? ops) nil
        :else (recur ((first ops) (make-image current)) (next ops))))))
;; M-x cljr-hotload-dependency RET [org.clojure/tools.trace "0.7.9"]
(use 'clojure.tools.trace)
(trace (* 2 3))
(trace-ns *ns*)
(untrace-ns *ns*)
;; C-c RET h d [spyscope "0.1.6"]
(use 'spyscope.core)
(test-all-moves (make-image '(".#" "##")) "#./## => .#./.../##.")
(test-all-moves (make-image '("..." "##." ".#.")) "#../.../#.# => .#../.#../#.#./####")
;; 14/02/2018
(defn make-image-from-4
  "Makes an image from 4 images as its corners in the following shape:
    ii | i
   ---------
    iii| iv "
  [[i ii iii iv :as all]]
  {:pre [(s/valid? (s/coll-of ::image :count 4) all)]
   :post [(s/valid? ::image %)]}
  (let [_ #spy/p i _ #spy/p ii _ #spy/p iii _ #spy/p iv fst-half (map concat (get-rows ii) (get-rows i))
        scnd-half (map concat (get-rows iii) (get-rows iv))]
    (make-image (concat fst-half scnd-half))))
(defn get-size [image]
  {:pre [(s/valid? ::image image)]
   :post [(s/valid? int? %)]}
  (let [rows (get-rows image)
        num-rows (count rows)
        column-lengths (map count rows)]
    (if (= num-rows (get (frequencies column-lengths) num-rows))
      num-rows
      (throw (ex-info "Irregular image" {:num-rows num-rows :column-lengths column-lengths})))))
(def r-of-pair (fn [[_ e]] e))
(def l-of-pair (fn [[e _]] e))
(defn divide-image-in-4
  "Gets the 4 corners of an image as 4 smaller images in the following way:
    ii | i
   ---------
    iii| iv "
  [image]
  {:pre [(s/valid? ::image image)]
   :post [(s/valid? (s/coll-of ::image :count 4) %)]}
  (let [mid (/ (get-size image) 2)
        fst-half #(take mid %)
        scnd-half #(drop mid %)
        image-rows (get-rows image)
        rows-pairs (map #(vector (fst-half %) (scnd-half %)) image-rows)
        fst-half-image (fst-half rows-pairs)
        scnd-half-image (scnd-half rows-pairs)]
    (vector (make-image (map r-of-pair fst-half-image))
            (make-image (map l-of-pair fst-half-image))
            (make-image (map l-of-pair scnd-half-image))
            (make-image (map r-of-pair scnd-half-image)))))
(divide-image-in-4 (make-image '(".#" "##")))
(make-image-from-4 (divide-image-in-4 (make-image '(".#.#" "...." "####" "#..#"))))
(defn apply-rules-atomic-image [initial-rules image]
  {:pre [(s/valid? ::image image) (s/valid? ::rules-list initial-rules)]
   :post [(s/valid? ::image %)]}
  (loop [rules initial-rules new-image nil]
    (cond
      (not (nil? new-image)) new-image
      (empty? rules) (throw (ex-info "No rule found!" {:image image}))
      :else (recur
             (rest rules)
             (test-all-moves image (first rules))))))
(defn apply-rules-image [image rules]
  {:pre [(s/valid? ::image image) (s/valid? ::rules rules)]
   :post [(s/valid? ::image %)]}
  (let [[i ii iii iv] (divide-image-in-4 image)]
    (make-image-from-4 [(increase-resolution i rules)
                        (increase-resolution ii rules)
                        (increase-resolution iii rules)
                        (increase-resolution iv rules)])))
(def ^:const THREES-PATTERN #"[\.#]{3}/[\.#]{3}/[\.#]{3}\s+=>\s+[\.#]{4}/[\.#]{4}/[\.#]{4}/[\.#]{4}")
(def ^:const TWOS-PATTERN #"[\.#]{2}/[\.#]{2}\s+=>\s+[\.#]{3}/[\.#]{3}/[\.#]{3}")
(s/def ::rule-two (s/and string? #(re-find TWOS-PATTERN %)))
(s/def ::rule-three (s/and string? #(re-find THREES-PATTERN %)))
(s/def ::twos (s/coll-of ::rule-two))
(s/def ::threes (s/coll-of ::rule-three))
(s/def ::rules-list (s/or :list-twos ::twos :list-threes ::threes))
(s/def ::rules (s/keys :req [::twos ::threes]))
(defn divisible-by [x n]
  (= 0 (mod x n)))
(s/def ::multiple-of-2 (s/and pos-int? #(divisible-by % 2)))
(s/def ::multiple-of-3 (s/and pos-int? #(divisible-by % 3)))
(s/def ::valid-image-size (s/or
                           :multiple-of-2 ::multiple-of-2
                           :multiple-of-3 ::multiple-of-3))
(defn divide-evenly [size]
  ;; {:pre [(s/valid? ::valid-image-size size)]
  ;;  :post [(s/valid? ::valid-image-size %)]}
  (cond
    (>= 3 size) size
    (divisible-by size 2) 2
    (divisible-by size 3) 3))
(defn increase-resolution [image {:keys [clips.core/twos clips.core/threes] :as rules}]
  {:pre [(s/valid? ::image image) (s/valid? ::rules rules)]
   :post [(s/valid? ::image %)]}
  (let [size (get-size image) split-size (divide-evenly size)]
    (-> (split-image image split-size)
        (apply-rules split-size rules)
        regroup-image)))
(def ^:const RES-TIMES 5)
(def ^:const INITIAL-IMAGE '(".#." "..#" "###"))
(-> "artist-rules"
    slurp
    clojure.string/split-lines
    (as-> rules-lines (reduce (fn [state el]
                                (if (= 5 (-> (clojure.string/split el #"/|(?:=>)")
                                             count))
                                  (update state ::twos conj el)
                                  (update state ::threes conj el)))
                              {::twos [] ::threes []}
                              rules-lines))
    (as-> rules (loop [times RES-TIMES image (make-image INITIAL-IMAGE)]
                  (if (= 0 times)
                    image
                    (recur (dec times) (increase-resolution image rules)))))
    get-rows
    (as-> image (transduce (comp
                            (map #(filter #{\#} %))
                            (map count))
                           (completing +)
                           image)))
;;17/02/2018
(defn split-image [image split-size]
  {:pre [(s/valid? ::image image)]
   ;; :post [(s/valid? (s/coll-of ::image) %)]
   }
  (if (= (get-size image) split-size)
    (vector image)
    (-> (partition split-size (get-rows image))
        (as-> rows (transduce (comp
                               (map #(map (partial partition split-size) %))
                               (map vec)
                               (map #(apply map
                                            (fn ([a b c] (make-image (list a b c)))
                                              ([a b] (make-image (list a b))))
                                            %)))
                              (completing into)
                              []
                              rows)))))
(defn apply-rules [images size rules]
  {:pre [;; (s/valid? (s/coll-of ::image) images)
         (s/valid? ::rules rules)]
   ;; :post [(s/valid? (s/every ::image) %)]
   }
  (let [{:keys [clips.core/twos clips.core/threes]} (s/conform ::rules rules)]
    (map (partial
          apply-rules-atomic-image
          (if (= 2 size) twos threes))
         images)))
;; C-c RET h d [org.clojure/math.numeric-tower "0.0.4"]
(require '[clojure.math.numeric-tower :as math])
(defn calculate-images-per-side [num]
  (math/sqrt num))
(defn regroup-image [initial-images]
  {:pre [;; (s/valid? (s/coll-of ::image) initial-images)
         ]
   :post [(s/valid? ::image %)]}
  (let [images-per-side (calculate-images-per-side (count initial-images))]
    (loop [rows [] images initial-images]
      (if (empty? images)
        (make-image rows)
        (recur (into rows (concat-images (take images-per-side images)))
               (drop images-per-side images))))))
(partition 2 (get-rows (make-image '(".." ".."))))
(split-image (make-image '("#.#.#.##." "#.#.#.##." "#.#.#.##." "#.#.#.##." "#.#.#.##." "#.#.#.##." "#.#.#.##." "#.#.#.##." "#.#.#.##.")) 3)
(split-image (make-image '("#.#.#.q" "#.#.#." "#.#.#." "#.#.#." "#.#.#." "#.#.#.")) 2)
(-> (map (partial partition 3) '("#.#.#.##." "#.#.#.##." "#.#.#.##."))
    vec
    (as-> r (map #(make-image (list %1 %2 %3)) (get r 0) (get r 1) (get r 2))))
(loop [times 3 rows [] list-img [[".#." "..." "###"] [".#." "..." "###"] [".#." "..." "###"]]]
  (if (= 0 times)
    (make-image rows)
    (recur (dec times)
           (into rows (apply (fn
                               ([[top-left top-centre top-right] [middle-left middle-centre middle-right] [bottom-left bottom-centre bottom-right]]
                                (vector
                                 (concat top-left middle-left bottom-left)
                                 (concat top-centre middle-centre bottom-centre)
                                 (concat top-right middle-right bottom-right)))
                               ([[top-left top-right] [bottom-left bottom-right]]
                                (vector
                                 (concat top-left bottom-left)
                                 (concat top-right bottom-right))))
                             (take 3 list-img)))
           (drop 3 list-img))))
;;18/02/2018
(test-all-moves (make-image '(".#." "#.." "###")) "##./#.#/#.. => .#../#.##/##.#/#.#.")
(defn concat-images [initial-images]
  (if (= 1 (count initial-images))
    (vec (first initial-images))
    (loop [images initial-images concatenation []]
      (if (empty? (first images))
        concatenation
        (recur (map (comp vec rest) images) (conj concatenation (reduce #(into %1 (first %2)) [] images)))))))
;; --- Day 21: Fractal Art --- Part 2
;; It won't be so easy, it'll need optimisation, but let's try :)
(def ^:const RES-TIMES 18)
;;It didn't
(defn apply-rules-atomic-image-opt [rules image]
  (let [match (get rules image)]
    (if (nil? match)
      (throw (ex-info "No rule found!" {:image image}))
      match)))
(def OPS-VECTOR [fx fy fx rot90 fx fy fx])
(-> "artist-rules"
    slurp
    clojure.string/split-lines
    (as-> rules-lines (reduce (fn [rules-map text-rule]
                                (let [rule-pieces (->> (clojure.string/replace text-rule #" " "")
                                                       (re-seq #"[\.#]+"))
                                      num-input-pieces (if (= 5 (count rule-pieces)) 2 3)
                                      match-image (make-image (drop num-input-pieces rule-pieces))]
                                  (loop [current-image (make-image (take num-input-pieces rule-pieces))
                                         ops OPS-VECTOR
                                         updated-rules-map rules-map]
                                    (if (empty? ops) 
                                      updated-rules-map
                                      (recur ((first ops) current-image)
                                             (next ops)
                                             (assoc
                                              updated-rules-map
                                              current-image
                                              match-image))))))
                              {}
                              rules-lines))
    (as-> rules (let [apply-rules-partial-opt (partial apply-rules-atomic-image-opt rules)]
                  (loop [times RES-TIMES image (make-image INITIAL-IMAGE)]
                    (if (= 0 times)
                      image
                      (recur (dec times) (let [size (get-size image)
                                               split-size (divide-evenly size)]
                                           (->> (split-image image split-size)
                                                (map apply-rules-partial-opt)
                                                regroup-image)))))))
    get-rows
    (as-> image (transduce (comp
                            (map #(filter #{\#} %))
                            (map count))
                           (completing +)
                           image)))
;; 21/02/2018
;; --- Day 22: Sporifica Virus --- Part 1
(def ^:const N [-1 0])
(def ^:const S [1 0])
(def ^:const E [0 1])
(def ^:const W [0 -1])
(def ^:const DIRS [N E S W])
(def ^:const BURSTS 10000000)
(-> "sporifica"
    slurp
;;  "..#
;; #..
;; ..."
    clojure.string/split-lines
    (as-> lines (hash-map :rows (count lines) :cols (frequencies (map count lines)) :lines lines)
      (assoc lines :current-pos [(-> lines
                                :rows
                                (/ 2)
                                int)
                            (-> (get-in lines [:cols (:rows lines)])
                                (/ 2)
                                int)])
      (assoc lines :infected (transduce (comp
                                         (map-indexed (fn [row-index full-row]
                                                         (map-indexed (fn [col-index node]
                                                                        (if (= \# node) [row-index col-index]))
                                                                      full-row)))
                                         (map #(remove nil? %)))
                                        (completing #(-> %2
                                                         set
                                                         (clojure.set/union %1)))
                                        #{}
                                        (:lines lines))))
    (assoc :current-dir 0) ;; 0 is the index for N in the DIRS vector
    (dissoc :rows)
    (dissoc :cols)
    (dissoc :lines)
    (as-> initial-state (loop [times BURSTS state initial-state]
                          (if (= 0 times)
                            state
                            (recur (dec times) (run-burst state)))))
    :infection-times
    prn)
(defn run-burst [{:keys [infected current-pos] :as state}]
  (-> (if (nil? (get infected current-pos))
        (-> state
            (update :infection-times (fnil inc 0))
            (update :infected conj current-pos)
            (update :current-dir turn-left))
        (-> state
            (update :infected disj current-pos)
            (update :current-dir turn-right)))
      (as-> updated-state
          (let [[row-dir col-dir] (get DIRS (:current-dir updated-state))]
            (update updated-state :current-pos (fn [[row col]]
                                                 (vector (+ row row-dir)
                                                         (+ col col-dir))))))))
(defn turn [to current]
  (mod (+ to current) 4))
(def turn-left (partial turn -1))
(def turn-right (partial turn 1))
;; 22/02/2018 -- Part 2
(time (-> "sporifica"
          slurp
           ;; "..#
;; #..
;; ..."
          clojure.string/split-lines
          (as-> lines (hash-map :rows (count lines) :cols (frequencies (map count lines)) :lines lines)
            (assoc lines :current-pos [(-> lines
                                           :rows
                                           (/ 2)
                                           int)
                                       (-> (get-in lines [:cols (:rows lines)])
                                           (/ 2)
                                           int)])
            (assoc lines :infected (transduce (comp
                                               (map-indexed (fn [row-index full-row]
                                                              (map-indexed (fn [col-index node]
                                                                             (if (= \# node) [row-index col-index]))
                                                                           full-row)))
                                               (map #(remove nil? %)))
                                              (completing #(-> %2
                                                               set
                                                               (clojure.set/union %1)))
                                              #{}
                                              (:lines lines))))
          (assoc :current-dir 0) ;; 0 is the index for N in the DIRS vector
          (dissoc :rows)
          (dissoc :cols)
          (dissoc :lines)
          (assoc :weakened #{})
          (assoc :flagged #{})
          (as-> initial-state (loop [times BURSTS state initial-state]
                                (if (= 0 times)
                                  state
                                  (recur (dec times) (run-burst-evolution state)))))
          :infection-times
          prn))
(defn run-burst-evolution [{:keys [infected weakened flagged current-pos] :as state}]
  (-> (cond
        (not (nil? (get infected current-pos))) (-> state
                                                    (update :infected disj current-pos)
                                                    (update :flagged conj current-pos)
                                                    (update :current-dir turn-right))
        (not (nil? (get weakened current-pos))) (-> state
                                                    (update :infection-times (fnil inc 0))
                                                    (update :weakened disj current-pos)
                                                    (update :infected conj current-pos))
        (not (nil? (get flagged current-pos))) (-> state
                                                    (update :flagged disj current-pos)
                                                    (update :current-dir turn-reverse))
        :else (-> state
                  (update :weakened conj current-pos)
                  (update :current-dir turn-left)))
      (as-> updated-state
          (let [[row-dir col-dir] (get DIRS (:current-dir updated-state))]
            (update updated-state :current-pos (fn [[row col]]
                                                 (vector (+ row row-dir)
                                                         (+ col col-dir))))))))
(def turn-reverse (partial turn 2))
;; Session 23/02/2018
;; --- Day 23: Coprocessor Conflagration --- Part 1
(defn str->num-or-register [str]
  (let [parsed (read-string str)]
    (if (number? parsed)
      parsed
      (keyword parsed))))
(defn get-val [regs val-or-reg]
  (if (keyword? val-or-reg)
    (get regs val-or-reg 0)
    val-or-reg))completing
(defmulti exec-next (fn [{:keys [pc instructions]}] (:instr (get instructions pc))))
(defmethod exec-next :set [{:keys [pc instructions registers] :as state}]
  (let [current-instr (get instructions pc)]
    (-> state
        (assoc-in [:registers (:p1 current-instr)] (get-val registers (:p2 current-instr)))
        (update :pc inc)
        (update-in [:instr-count :set] (fnil inc 0)))))
(defmethod exec-next :sub [{:keys [instructions pc registers] :as state}]
  (let [current-instr (get instructions pc)]
    (-> state
        (update-in [:registers (:p1 current-instr)] (fnil - 0) (get-val registers (:p2 current-instr)))
        (update :pc inc)
        (update-in [:instr-count :sub] (fnil inc 0)))))
(defmethod exec-next :mul [{:keys [instructions pc registers] :as state}]
  (let [current-instr (get instructions pc)]
    (-> state
        (update-in [:registers (:p1 current-instr)] (fnil * 0) (get-val registers (:p2 current-instr)))
        (update :pc inc)
        (update-in [:instr-count :mul] (fnil inc 0)))))
(defmethod exec-next :jnz [{:keys [instructions pc registers] :as state}]
  (let [current-instr (get instructions pc)]
    (-> (if (not= 0 (get-val registers (:p1 current-instr)))
          (update state :pc + (get-val registers (:p2 current-instr)))
          (update state :pc inc))
        (update-in [:instr-count :jnz] (fnil inc 0)))))
(defmethod exec-next :default [state] (assoc state :stop true))
(defn run-coprocessor-instr [initial-state]
  (loop [state initial-state]
    (if (:stop state)
      state
      (recur (exec-next state)))))
(->> "coprocessor"
    slurp
    (clojure.string/split-lines)
    (into []
          (comp
           (map #(clojure.string/split % #" "))
           (map (fn [[instr p1 p2]]
                  (hash-map :instr (keyword instr) :p1 (str->num-or-register p1) :p2 (str->num-or-register p2))))))
    (hash-map :pc 0 :registers {} :instr-count {} :instructions)
    run-coprocessor-instr
    :mul
    prn)
(clojure.string/split "jnz 1 -23" #" ")
((fn [[a b c]] (prn a b c)) '(:a :b :c))
(inc nil)
;; -- Part 2
;; Attempt 1: Let's see if ther are infinite loops
(defn run-coprocessor-instr-interrupt-cycles [initial-state]
  (loop [state initial-state
         times COPROC-TIMES
         max-pc -1
         internal-states #{}]
    (let [_ (some-> (resolve 'COPROC-DEBUG)
                    deref
                    (#(if % (clojure.pprint/pprint (get state :registers)))))
          current-internal-state (-> state (dissoc :instr-count) (dissoc :instructions))]
      (cond
        (:stop state) (assoc state :exit-code :end-program)
        (not (nil? (get internal-states current-internal-state))) (-> state
                                                                      (assoc :exit-code :cycle)
                                                                      (assoc :cycle-state current-internal-state))
        (= times 0) (-> state
                        (assoc :exit-code :time-out)
                        (assoc :max-pc max-pc))
        :else (recur
               (exec-next state)
               (dec times)
               (max (:pc state) max-pc)
               (conj internal-states current-internal-state))))))
(->> "coprocessor"
    slurp
    (clojure.string/split-lines)
    (into []
          (comp
           (map #(clojure.string/split % #" "))
           (map (fn [[instr p1 p2]]
                  (hash-map :instr (keyword instr) :p1 (str->num-or-register p1) :p2 (str->num-or-register p2))))))
    (hash-map :pc 0 :registers {:a 0} :instr-count {} :instructions)
    run-coprocessor-instr-interrupt-cycles
    (#(dissoc % :instructions))
    prn
    )
;; Attempt 2: Let's see if it is possible to see what the program is doing checking its state
(def ^:const COPROC-DEBUG true)
@(resolve 'COPROC-DEBUG)
(def ^:const COPROC-TIMES 100000)
;; Attempt 3: # non-primes on 109300, 109317, 109334, ..., 109300 + n*17 < 126300
(defn make-lazy-multiples-of [factor]
  (fn lazy-generator [seed]
    (lazy-seq
     (cons seed
           (lazy-seq (lazy-generator (+ seed factor)))))))
(take 3 ((make-lazy-multiples-of 17) 109300))
(defn prime? [n]
  (and
   (not= n 1)
   (= ((fn smallest-divisor [n test]
         (cond
           (> (* test test) n) n
           (= (mod n test) 0) test
           :else (smallest-divisor n (inc test)))) n 2) n)))
(count (sequence (comp
                  (take-while #(< % 126300))
                  ;;(remove prime?)
                  )
                 ((make-lazy-multiples-of 17) 109300)))
;;910 Too low!
;;24/02/2018
(/ (- 126300 109300) 17)
(count (sequence (comp
                  (take-while #(not= % 126300))
                  (remove prime?))
                 ((make-lazy-multiples-of 17) 109300)))
(count (sequence (comp
                  (take-while #(not= % (+ 10 (* 10 17))))
                  (remove prime?)
                  )
                 ((make-lazy-multiples-of 17) 10)))
;;desperate measures
(def ^:const B 109300
  ;;10
  )
(def ^:const C 126300
  ;;(+ B (* 1000 17))
  )
(time (let [c C]
        (loop [b B h 0]
          (if (= b c)
            h
            (recur (+ b 17) (loop [d 2 f 1]
                              (cond
                                (= 0 f) (inc h)
                                (= d b) h
                                :else (recur (inc d) (loop [e 2 ff f]
                                                       (if (or (= 0 ff) (= e b))
                                                         ff
                                                         (recur (inc e) (if (= b (* d e)) 0 1))))))))))))
;; C-c RET h d [org.clojure/math.numeric-tower "0.0.4"]
(require '[clojure.math.numeric-tower :as math])
(time (let [c C]
        (loop [b B h 0]
          (if (= b c)
            h
            (recur (+ b 17) (loop [d 2 f 1]
                              (cond
                                (= 0 f) (inc h)
                                (> (sq d) b) h
                                :else (recur (inc d) (loop [e d ff f]
                                                       (if (or (= 0 ff) (> (* d e) b))
                                                         ff
                                                         (recur (inc e) (if (= b (* d e)) 0 1))))))))))))
;; 910!
(def sq (memoize (fn [n] (* n n))))
;; The answer was 911!!!!!!! Why?!
(sequence (comp
           (take-while #(not= % 126300))
           (remove prime?))
          ((make-lazy-multiples-of 17) 109300))
;; Aaaah, I seeeee! It needs to run through [109300, 126300] and I was running it through [109300, 126300)
(count (sequence (comp
                  (take-while #(<= % 126300))
                  (remove prime?))
                 ((make-lazy-multiples-of 17) 109300)))
;;25/02/2018
;; --- Day 24: Electromagnetic Moat --- Part 1
(defn make-bcomp [a b]
  {:a a :b b})
(defn a [bcomp]
  (:a bcomp))
(defn b [bcomp]
  (:b bcomp))
(defn compat? [n bcomp]
  (or (= n (a bcomp)) (= n (b bcomp))))
(defn possible-bridges [bcomps port path]
  (loop [candidates (filter (partial compat? port) bcomps) bridges '()]
    (if (empty? candidates)
      (conj bridges path)
      (let [current (first candidates)]
        (recur
         (rest candidates)
         (into bridges (possible-bridges
                        (disj bcomps current)
                        (other-port current port)
                        (conj path current))))))))
(defn other-port [bcomp port]
  (let [a (a bcomp) b (b bcomp)]
    (if (= a port)
      b
      a)))
(defn get-strength [bridge]
  (hash-map :bridge bridge :strength (reduce #(+ %1 (a %2) (b %2)) 0 bridge)))
(-> "bridge-components"
 slurp
 ;; "0/2
;; 2/2
;; 2/3
;; 3/4
;; 3/5
;; 0/1
;; 10/1
;; 9/10"
    clojure.string/split-lines
    (as-> lines (into #{} (comp
                           (map #(re-seq #"[0-9]+" %))
                           (map #(map read-string %))
                           (map (fn [[a b]] (make-bcomp a b))))
                      lines))
    (possible-bridges 0 '())
    (as-> bridges
        (remove empty? bridges)
      (map get-strength bridges)
      (sort-by :strength bridges))
    last)
(re-find #"[0-9]+" "99 67")
(re-seq #"[0-9]+" "99 67")
(conj '() (list (list :a :b) (list :b :c)))
(into '() (list (list :a :b) (list :b :c)))
;; -- Part 2
(-> "bridge-components"
 slurp
 ;; "0/2
;; 2/2
;; 2/3
;; 3/4
;; 3/5
;; 0/1
;; 10/1
;; 9/10"
    clojure.string/split-lines
    (as-> lines (into #{} (comp
                           (map #(re-seq #"[0-9]+" %))
                           (map #(map read-string %))
                           (map (fn [[a b]] (make-bcomp a b))))
                      lines))
    (possible-bridges 0 '())
    (as-> bridges
        (remove empty? bridges)
      (sort-by count bridges)
      (let [max-length (-> bridges last count)]
        (filter #(= max-length (count %)) bridges))
      (map get-strength bridges)
      (sort-by :strength bridges))
    last)
;; --- Day 25: The Halting Problem --- Part 1
(def ^:const STATE {:initial-state :a
                    :diag-checksum 12964419
                    :states {:a {0 {:output 1
                                    :dir :right
                                    :next-state :b}
                                 1 {:output 0
                                    :dir :right
                                    :next-state :f}}
                             :b {0 {:output 0
                                    :dir :left
                                    :next-state :b}
                                 1 {:output 1
                                    :dir :left
                                    :next-state :c}}
                             :c {0 {:output 1
                                    :dir :left
                                    :next-state :d}
                                 1 {:output 0
                                    :dir :right
                                    :next-state :c}}
                             :d {0 {:output 1
                                    :dir :left
                                    :next-state :e}
                                 1 {:output 1
                                    :dir :right
                                    :next-state :a}}
                             :e {0 {:output 1
                                    :dir :left
                                    :next-state :f}
                                 1 {:output 0
                                    :dir :left
                                    :next-state :d}}
                             :f {0 {:output 1
                                    :dir :right
                                    :next-state :a}
                                 1 {:output 0
                                    :dir :left
                                    :next-state :e}}}})
(loop [times (:diag-checksum STATE)
       machine-state {:state (:initial-state STATE) :tape {:pos 0 :cont [0]}}]
  (if (= 0 times)
    (reduce + (get-in machine-state [:tape :cont]))
    (recur (dec times) (turing-step machine-state STATE))))
(defn move-right [{:keys [cont pos] :as tape}]
  (as-> tape updated-tape
    (update updated-tape :pos inc)
    (if (nil? (get cont (inc pos)))
        (update updated-tape :cont conj 0)
        updated-tape)))
(defn move-left [{:keys [cont pos] :as tape}]
  (if (= 0 pos)
    (assoc tape :cont (-> cont
                          sequence
                          (conj 0)
                          vec))
    (update tape :pos dec)))
(defn get-current-on-tape [{:keys [cont pos] :as tape}]
  (get cont pos))
(defn turing-step [{{:keys [cont pos]} :tape :keys [state tape] :as current} {:keys [states]}]
  (let [c-val (get-current-on-tape tape)
        c-state (get states state)
        c-action (get c-state c-val)]
    (-> current
        (assoc :state (:next-state c-action))
        (assoc :tape (as-> tape updated-tape
                       (assoc-in updated-tape [:cont pos] (:output c-action))
                       (if (= :left (:dir c-action))
                         (move-left updated-tape)
                         (move-right updated-tape)))))))
(cons :b [:a])
(cons :b '(:a))
(-> {:pos 0 :cont [:a :b :c]}
    move-left
    move-left
    move-right
    move-left
    move-right
    move-right
    move-right
    move-right
    move-right)
(into '() "string")
(into [] "string")
(into #{} "string")
(into {} (repeatedly 4 #(vector (rand-int 100) (rand-int 100))))
(into {} '((:a 1) (:b 2))) ;; hmm it needs a vector [k v]
(into {} (repeatedly 4 #(vector (rand-int 100) (rand-int 100) (rand-int 100)))) ;; and it needs to be excatly [k v] nothing else
;;26/02/2018
(read-line)
(take 8 (cycle [1 2 3 4]))
(-> "0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10"
    clojure.string/split-lines
    (as-> lines (into #{} (comp
                           (map #(re-seq #"[0-9]+" %))
                           (map #(map read-string %))
                           (map (fn [[a b]] (make-bcomp a b))))
                      lines))
    (possible-bridges 0 '())
    (as-> bridges (apply max-key (fn [elem] (reduce #(+ %1 (a %2) (b %2)) 0 elem)) bridges)))
;;27/02/2018
;;#69 Merge with a Function
(defn my-merge-with [f & maps]
  (reduce (fn [state elem]
            (into state (reduce
                         (fn [inner-state [k v]]
                           (let [existing-val (get state k)]
                             (if (some? existing-val)
                               (assoc inner-state k (f existing-val v))
                               (assoc inner-state k v))))
                         {}
                         elem)))
          {}
          maps))
(= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})

(= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})

(= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})

;;#70 Word sorting
(sort '("a" "bb"))
(compare "a" "bb")
(compare "Have" "a")
(compare "have" "a")
(compare "a" "A")
(compare "A" "a")
(defn ms [s]
  (as-> s st
    (clojure.string/replace st #"[\.!,;]" "")
    (clojure.string/split st #" ")
    (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) st)))

(= (ms  "Have a nice day.")
   ["a" "day" "Have" "nice"])

(= (ms  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])

(= (ms  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])
;;28/02/2018
;;#73 Analyze a Tic-Tac-Toe Board
(vec (repeat w (vec (repeat h nil))))
(count (filter #(get-in board %) (neighbours loc)))
(let [w (count board)
      h (count (first board))]
  (loop [new-board board x 0 y 0]
    (cond
      (>= x w) new-board
      (>= y h) (recur new-board (inc x) 0)
      :else
      (let [new-liveness
            (case (count-neighbours board [x y])
              2 (get-in board [x y])
              3 :on
              nil)]
        (recur (assoc-in new-board [x y] new-liveness) x (inc y))))))
(or 2 1)
(or false 1)
(or nil 1)
(defn window
"Returns a lazy sequence of 3-item windows centered around each item of coll."
  [coll]
  (partition 3 1 (concat [nil] coll [nil])))
(defn cell-block
"Creates a sequences of 3x3 windows from a triple of 3 sequences."
  [[left mid right]]
  (window (map vector
               (or left (repeat nil))
               mid
               (or right (repeat nil)))))
(apply distinct? '(:x :o :e))
(apply distinct? '(:o :o :o))
(remove #(apply distinct? %) '((:x :o :e)))
(remove #(apply distinct? %) '((:o :o :o)))
(defn analyze [board]
  (-> '()
      (into board)
      (into (apply map (fn [x y z] (list x y z)) board))
      (into (map (fn [[a b c]] (list (get-in board a)
                                     (get-in board b)
                                     (get-in board c)))
                 '(([0 0] [1 1] [2 2]) ([0 2] [1 1] [2 0]))))
      (as-> lines
          (filter #(= 1 (count (frequencies %))) lines)
        (filter #(not-any? #{:e} %) lines))
      ffirst))
(ffirst '((:e :e :e)))
(remove #(#{:e} %) '((:e :e :e)))
(not-any? #{:e} '(:e :e :e))
(not-any? #{:e} '(:x :o :o))
(map #(apply distinct? %) '((:e :o :e) (:o :o :o) (:e :e :o) (:e :o :o) (:o :o :e) [:e :o :o] [:o :o :e] [:o :e :e]))
(apply distinct? (list :o :o :o))
(apply distinct? (list :e :o :e))
(apply distinct? '(:o :o :o))
(apply distinct? '(:e :o :e))
(analyze [[:o :e :e] [:o :o :e] [:e :o :o]])
(analyze [[:o :e :e] [:e :e :e] [:e :e :e]])
(= nil (analyze [[:e :e :e]
            [:e :e :e]
            [:e :e :e]]))

(= :x (analyze [[:x :e :o]
           [:x :e :e]
           [:x :e :o]]))

(= :o (analyze [[:e :x :e]
           [:o :o :o]
           [:x :e :x]]))

(= nil (analyze [[:x :e :o]
            [:x :x :e]
            [:o :x :o]]))

(= :x (analyze [[:x :e :e]
           [:o :x :e]
           [:o :e :x]]))

(= :o (analyze [[:x :e :o]
           [:x :o :e]
           [:o :e :x]]))

(= nil (analyze [[:x :o :x]
            [:x :o :x]
            [:o :x :o]]))
((fn [lines] (filter #(= 1 (count (frequencies %))) lines)))
((fn [lines] (filter #(not-any? #{:e} %) lines)))
