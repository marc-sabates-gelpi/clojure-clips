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
