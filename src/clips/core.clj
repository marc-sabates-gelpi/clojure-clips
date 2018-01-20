(ns clips.core
  (:require [clojure.core.async :as async]))
;;12/12/2017
(inst-ms (read-string #inst "2017-10-26T19:21:57.090-00:00"))
(inst-ms #inst "2017-10-26T19:21:57.090-00:00")
;;13/12/2017
;;Advent of code 2017 - 1: Inverse captcha
((fn part1 [i]
   (->> i
        str
        (sequence (comp (map str) (map read-string)))
        vec
        (#(conj % (first %)))
        (partition 2 1 '(nil))
        (transduce
         (comp
          (filter (fn [[a b]] (= a b)))
          (map first))
         +)))
 951484596541141557316984781494999179679767747627132447513171626424561779662873157761442952212296685573452311263445163233493199211387838461594635666699422982947782623317333683978438123261326863959719777179228599319321138948466562743761584836184512984131635354116264899181952748224523953976485816295227945792555726121913344959454458829485471174415775278865324142733339789878929596275998341778873889585819916457474773252249179366599951454182657225576277834669222982366884688565754691273745959468648957498511326215934353963981471593984617554514519623785326888374742147318993423214834751785956958395133486656388454552769722562524415715913869946325551396638593398729938526424994348267935153555851552287223313383583669912941364344694725478258297498969517632881187394141593479818536194597976519254215932257653777455227477617957833273463216593642394215275314734914719726618923177918342664351954252667253233858814365351722938716621544226598956257753212248859258351363174782742336961425325381561575992352415514168782816173861148859478285339529151631429536819286498721812323861771638574344416879476255929929157912984151742613268754779685396125954595318134933366626594498249956388771723777242772654678448815844555372892574747735672368299826548254744359377667294764559334659523233146587568261116253155189394188696831691284711264872914348961888253386971994431352474717376878745948769171243242621219912378731755544387249443997382399714738351857752329367997665166956467544459817582915478514486541453932175598413554259672117364863112592515988922747164842668361925135551248923449968328385889877512156952725198691746951431443497496455761516486573476185321748523644283494181119399874324683922393547682851931435931276267766772798261563117954648576421741384823494187895272582575669685279986988357796138794326125852772995446355723211161523161886222562853546488411563473998633847953246787557146187696947831335722888918172961256498971868946237299523474841983527391489962357196433927251798764362493965894995592683296651874787384247326643886774966828657393717626591578321174832222434128817871765347278152799425565633521152643686221411129463425496425385516719682884157452772141585743166647191938727971366274357874252166721759N)
(= '(1 2))
(def xt (comp
         (filter (fn [[a b]] (= a b)))
         (map first)))
(eduction xt '((1 1) (1 2) (2 2) (2 1) (1 nil)))
(sequence "abc")
(map read-string (map str (sequence (str 123))))
(take 1 1234)
(read-string \1)
(sequence (str 123))
(defn part2
  [i]
  (->> i
        str
        (sequence (comp (map str) (map read-string)))
        (#(apply interleave (split-at (/ (count %) 2) %)))
        (partition 2 2)
        (transduce
         (comp
          (filter (fn [[a b]] (= a b)))
          (map first)
          (map (partial * 2)))
         +)))
(#(partition 2 (/ (count %) 2) '(nil) %) '(1 2 1 2))
(take-nth 2 '(1 2 3 4))
(take-nth 3 '(1 2 3 4))
(partition 2 3 '(nil) '(1 2 3 4))
(#(split-at (/ (count %) 2) %) '(1 2 3 4))
(apply interleave (#(split-at (/ (count %) 2) %) '(1 2 1 2)))
(part2 1212)
(part2 1221)
(part2 123425)
(part2 123123)
(part2 12131415)
(part2 951484596541141557316984781494999179679767747627132447513171626424561779662873157761442952212296685573452311263445163233493199211387838461594635666699422982947782623317333683978438123261326863959719777179228599319321138948466562743761584836184512984131635354116264899181952748224523953976485816295227945792555726121913344959454458829485471174415775278865324142733339789878929596275998341778873889585819916457474773252249179366599951454182657225576277834669222982366884688565754691273745959468648957498511326215934353963981471593984617554514519623785326888374742147318993423214834751785956958395133486656388454552769722562524415715913869946325551396638593398729938526424994348267935153555851552287223313383583669912941364344694725478258297498969517632881187394141593479818536194597976519254215932257653777455227477617957833273463216593642394215275314734914719726618923177918342664351954252667253233858814365351722938716621544226598956257753212248859258351363174782742336961425325381561575992352415514168782816173861148859478285339529151631429536819286498721812323861771638574344416879476255929929157912984151742613268754779685396125954595318134933366626594498249956388771723777242772654678448815844555372892574747735672368299826548254744359377667294764559334659523233146587568261116253155189394188696831691284711264872914348961888253386971994431352474717376878745948769171243242621219912378731755544387249443997382399714738351857752329367997665166956467544459817582915478514486541453932175598413554259672117364863112592515988922747164842668361925135551248923449968328385889877512156952725198691746951431443497496455761516486573476185321748523644283494181119399874324683922393547682851931435931276267766772798261563117954648576421741384823494187895272582575669685279986988357796138794326125852772995446355723211161523161886222562853546488411563473998633847953246787557146187696947831335722888918172961256498971868946237299523474841983527391489962357196433927251798764362493965894995592683296651874787384247326643886774966828657393717626591578321174832222434128817871765347278152799425565633521152643686221411129463425496425385516719682884157452772141585743166647191938727971366274357874252166721759N)
;;14/12/2017
(time (->> (slurp "checksum.txt")
           (clojure.string/split-lines)
           (transduce (comp
                       (map #(clojure.string/split % #"\t"))
                       (map #(reduce
                              (fn [{:keys [minimum maximum]} elem]
                                (let [current (read-string elem)]
                                  {:minimum (min minimum current) :maximum (max maximum current)}))
                              {:minimum 2147483647 :maximum -2147483646}
                              %))
                       (map #(- (:maximum %) (:minimum %))))
                      +)))
(time (->> (slurp "checksum.txt")
            (clojure.string/split-lines)
            (transduce (comp
                        (map #(clojure.string/split % #"\t"))
                        (map #(map read-string %))
                        (map sort)
                        (map #(hash-map :min (first %) :max (last %)))
                        (map #(- (:max %) (:min %))))
                       +)))
((fn [coll]
   (partial
    (fn [coll elem]
      (some
       #(and
         (= (mod elem %) 0)
         (not= elem %)) coll))
    coll))
 '(2 3 4))
;;04/01/2018
;;Advent of code - day 2
(->> (slurp "checksum.txt")
            (clojure.string/split-lines)
            (eduction (comp
                        (map #(clojure.string/split % #"\t"))
                        (map #(map read-string %))
                        (map sort))))
(defn evenly-divisor? [divisor dividend]
  (and (= 0 (mod dividend divisor)) (not= 1 divisor) (not= dividend divisor)))
(evenly-divisor? 1 2)
(evenly-divisor? 2 3)
(evenly-divisor? 8 2)
(evenly-divisor? 8 8)
(evenly-divisor? 2 8)
(defn find-evenly-divisor [coll]
  (first (remove nil? (for [a coll b coll]
                        (when (evenly-divisor? a b) (/ b a))))))
(find-evenly-divisor '(5 9 2 8))
(->> (slurp "checksum.txt")
            (clojure.string/split-lines)
            (transduce (comp
                        (map #(clojure.string/split % #"\t"))
                        (map #(map read-string %))
                        (map sort)
                        (map find-evenly-divisor))
                       +))
;; Advent of code --- Day 3: Spiral Memory ---
;;05/01/2018
(defn sq [n] (* n n))
(defn side [l] (inc (* (dec l) 2)))
(defn top-el [l] (sq (side l)))
(defn num-el [l] (if (> l 1) (* 4 (dec (side l))) 1))
(defn nums [l] {:from (inc (- (top-el l) (num-el l))) :to (top-el l)})
(defn find-level [n]
  (loop [cand 1]
    (let [nums (nums cand) from (:from nums) to (:to nums)]
      (if (<= from n to)
        cand
        (recur (inc cand))))))
(defn all [l]
  {:side (side l) :top-el (top-el l) :num-el (num-el l) :nums (nums l)})
(all 1)
(all 2)
(all 5)
(find-level 368078)
(all 304)
(defn abs [a]
  (if (> 0 a)
    (* -1 a)
    a))
(defn mh-dist [[x y]]
  (+ (abs x) (abs y)))
(mh-dist '(0 0))
(mh-dist '(-1 0))
(mh-dist '(-2 3))
(range (:from (nums 3)) (inc (:to (nums 3))))
(defn corners [l]
  (if (> l 1)
    (map first (partition (dec (side l)) (reverse (range (:from (nums l)) (inc (:to (nums l)))))))
    1))
(corners 3)
(corners 2)
(corners 4)
(defn corner? [n]
  (some? (some #{n} (corners (find-level n)))))
(corner? 9)
(defn mem-dist [n]
  (let [l (find-level n)]
    (if (corner? n)
      l
      (dec l))))
(mem-dist 9)
(mem-dist 8)
(mem-dist 10)
(defn get-h-dist [n]
  (let [l (find-level n)
        s (side l)
        nums (nums l)
        vals (first
              (filter #(some #{n} %) (partition-all
                                      s
                                      (dec s)
                                      (reverse (range (:from nums) (inc (:to nums)))))))]
    (get
     (zipmap
      vals
      (d vals l))
     n)))
((fn [l]
   (let [coll (reverse (range (:from (nums l)) (inc (:to (nums l))))) min (dec (:from (nums l)))]
     {:nums coll :h-val (map #(- % min) coll)})) 3)
(map #(filter #{2} %) '((1 2 3) (4 5 6)))
(filter #(some #{6} %) '((1 2 3) (4 5 6)))
;;06/01/2018
(side 5)
(defn round-down [n]
  (int n))
(round-down 1)
(round-down 1.5)
(round-down 1.7)
(round-down (/ 11 5))
(defn d [coll l]
  (let [s (side l) max (round-down (/ s 2))]
    (map abs (take s (iterate inc (* -1 max))))))
(d '(9 8 7) 2)
(get-h-dist 81)
(defn mh-dist [n]
  (+ (dec (find-level n)) (get-h-dist n)))
(mh-dist 9)
(mh-dist 10)
(mh-dist 25)
(mh-dist 368078)
;;08/01/2018
;; Advent of code --- Day 3: Spiral Memory (2) ---
(defn spiral-add-level
  ([] (spiral-add-level nil))
  ([s]
   (letfn [(size [spiral] (-> spiral first count))
           (make-vector [size] (vec (repeat size nil)))
           (inc-size [n] (inc (inc n)))
           (expand-horizontal [spiral] (map #(-> %
                                                 (conj nil)
                                                 (as-> coll
                                                       (apply list coll)
                                                       (cons nil coll))
                                                 vec)
                                            spiral))]
     (if (nil? s)
       (vector (vector nil))
       (let [c-size (size s) n-size (inc-size c-size)]
         (-> []
             (conj (make-vector n-size))
             (into (expand-horizontal s))
             (conj (make-vector n-size))))))))
(clojure.pprint/pprint (vec (repeat 3 (vec (repeat 3 nil)))))
(clojure.pprint/pprint (spiral-add-level [[nil]]))
(clojure.pprint/pprint (spiral-add-level (spiral-add-level [[nil]])))
(vector (vector nil))
(-> (vector (vector nil)) flatten)
(ffirst [[nil]])
(first (vec (map #(vector nil (first %) nil) (first [[nil nil nil] [nil nil nil] [nil nil nil]]))))
(map #(vector nil % nil) [[nil nil nil] [nil nil nil] [nil nil nil]])
(map #(assoc (conj % nil) 0 nil) [[nil nil nil] [nil nil nil] [nil nil nil]])
(conj [nil nil nil] nil)
(cons nil (apply list (conj [[1 2 3]] nil)))
((fn expand-horizontal [spiral] (vec (map #(vec (cons nil (apply list (conj % nil)))) spiral))) [[nil nil nil] [nil nil nil] [nil nil nil]])
(into '(1 2) '(9 10))
(into [1 [2]] [9 10])
(clojure.pprint/pprint (spiral-add-level [[5 4 2] [10 1 1] [11 23 25]]))
(vec (repeat 5 nil))
(vector [\a \c] (repeat 5 nil))
(vector (vec (repeat 5 nil)))
(clojure.pprint/pprint (spiral-add-level [[17 16 15 14 13] [18 5 4 3 12] [19 6 1 2 11] [20 7 8 9 10] [21 22 23 24 25]]))
(defn spiral-add-next [spiral n]
  (letfn [(find-next-pos [spiral] (let [spiral-size (count spiral)
                                        last-column-nils (count (filter #(nil? (last %)) (butlast spiral)))
                                        first-row-nils (count (filter nil? (first spiral)))
                                        first-column-nils (count (filter #(nil? (first %)) spiral))
                                        last-row-nils (count (filter nil? (last spiral)))]
                                    (cond
                                      (< 0 last-column-nils) (list (dec last-column-nils) (dec spiral-size))
                                      (< 0 first-row-nils) (list 0 (dec first-row-nils))
                                      (< 0 first-column-nils) (list (- spiral-size first-column-nils) 0)
                                      (< 0 last-row-nils) (list (dec spiral-size) (- spiral-size last-row-nils))
                                      :else (let [center (round-down (/ spiral-size 2))] (list center center)))))]
    (if (not-any? nil? (flatten spiral))
      (let [side (inc (count spiral))]
        (assoc-in (spiral-add-level spiral) [(dec side) side] n))
      (let [next-pos (find-next-pos spiral)] (assoc-in spiral [(first next-pos) (last next-pos)] n)))))
(-> [[17 16 15 14 13] [18 5 4 3 12] [19 6 1 2 11] [20 7 8 9 10] [21 22 23 24 25]]
    (spiral-add-next 26)
    (spiral-add-next 27)
    (spiral-add-next 28)
    (spiral-add-next 29)
    (spiral-add-next 30)
    (spiral-add-next 31)
    (spiral-add-next 32)
    (spiral-add-next 33)
    (spiral-add-next 34)
    (spiral-add-next 35)
    (spiral-add-next 36)
    (spiral-add-next 37)
    (spiral-add-next 38)
    (spiral-add-next 39)
    (spiral-add-next 40)
    (spiral-add-next 41)
    (spiral-add-next 42)
    (spiral-add-next 43)
    (spiral-add-next 44)
    (spiral-add-next 45)
    (spiral-add-next 46)
    (spiral-add-next 47)
    (spiral-add-next 48)
    (spiral-add-next 49)
    (spiral-add-next 50)
    (spiral-add-next 51)
    (spiral-add-next 52)
    (spiral-add-next 53)
    (spiral-add-next 54)
    (spiral-add-next 55)
    (spiral-add-next 56)
    (spiral-add-next 57)
    (spiral-add-next 58)
    (spiral-add-next 59)
    (spiral-add-next 60)
    (spiral-add-next 61)
    (spiral-add-next 62)
    (spiral-add-next 63)
    (spiral-add-next 64)
    (spiral-add-next 65)
    (spiral-add-next 66)
    (spiral-add-next 67)
    (spiral-add-next 68)
    (spiral-add-next 69)
    (spiral-add-next 70)
    (spiral-add-next 71)
    (spiral-add-next 72)
    (spiral-add-next 73)
    (spiral-add-next 74)
    (spiral-add-next 75)
    clojure.pprint/pprint)
;;10/01/2017
;; Advent of code --- Day 3: Spiral Memory (2) ---
(defn add-neighbours [spiral loc]
  (let [val-step (comp
                  (map #(get-in spiral %))
                  (remove nil?))]
    (transduce val-step + (neighbours-pos loc))))
(defn neighbours-pos
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))
(neighbours-pos [0 2])
(add-neighbours [[nil nil 2] [nil 1 1] [nil nil nil]] [0 1])
(defn spiral-add-next-stress-test [spiral]
  (letfn [(find-next-pos [spiral] (let [spiral-size (count spiral)
                                        last-column-nils (count (filter #(nil? (last %)) (butlast spiral)))
                                        first-row-nils (count (filter nil? (first spiral)))
                                        first-column-nils (count (filter #(nil? (first %)) spiral))
                                        last-row-nils (count (filter nil? (last spiral)))]
                                    (cond
                                      (< 0 last-column-nils) (list (dec last-column-nils) (dec spiral-size))
                                      (< 0 first-row-nils) (list 0 (dec first-row-nils))
                                      (< 0 first-column-nils) (list (- spiral-size first-column-nils) 0)
                                      (< 0 last-row-nils) (list (dec spiral-size) (- spiral-size last-row-nils))
                                      :else (let [center (round-down (/ spiral-size 2))] (list center center)))))]
    (if (not-any? nil? (flatten spiral))
      (let [side (inc (count spiral))
            spiral-new-level-opened (spiral-add-level spiral)
            pos [(dec side) side]
            val (add-neighbours spiral-new-level-opened pos)]
        [(assoc-in spiral-new-level-opened pos val) val])
      (let [next-pos (find-next-pos spiral)
            ;; pos [(first next-pos) (last next-pos)]
            pos (vec next-pos)
            val (add-neighbours spiral pos)]
        [(assoc-in spiral pos val) val]))))
(defn find-value [spiral n]
  (loop [filling-spiral spiral]
    (let [[spiral-tmp cand] (spiral-add-next-stress-test filling-spiral)]
      (if (>= cand n)
        cand
        (recur spiral-tmp)))))
(find-value [[1]] 368078)
;;--- Day 4: High-Entropy Passphrases ---
(-> "passphrases.txt"
     slurp
     clojure.string/split-lines
     (as-> coll (sequence (comp
                           (map #(clojure.string/split % #" "))
                           (filter #(= (count %) (count (distinct %)))))
                          coll))
     count)
(distinct (aaa aaa))
(-> "passphrases.txt"
     slurp
     clojure.string/split-lines
     (as-> coll (sequence (comp
                           (map #(clojure.string/split % #" "))
                           (map #(sequence (comp
                                            (map sequence)
                                            (map sort)
                                            (map clojure.string/join))
                                           %))
                           (filter #(= (count %) (count (distinct %)))))
                          coll))
     count)
(clojure.string/join (sort (sequence "abca")))
;;--- Day 5: A Maze of Twisty Trampolines, All Alike ---
(defn count-steps [coll]
  (let [total (count coll)]
    (loop [working-coll (vec coll) index 0 steps 1]
      (let [current (get working-coll index)]
        (if (= (- total index current) 0)
          steps
          (recur (update working-coll index inc) (+ index current) (inc steps)))))))
(-> "jumps.txt"
    slurp
    clojure.string/split-lines
    (as-> coll (map read-string coll))
    count-steps)
(clojure.string/replace "a b c" \space \newline)
(get 0 '(1))
(- 4 2 1)
(get [1] 0 )
(defn next-jump-op [offset]
  (if (>= offset 3)
    dec
    inc))
(defn count-steps [coll]
  (let [total (count coll)]
    (loop [working-coll (vec coll) index 0 steps 1]
      (let [current (get working-coll index)]
        (if (= (- total index current) 0)
          steps
          (recur (update working-coll index (next-jump-op current)) (+ index current) (inc steps)))))))
(-> "jumps.txt"
    slurp
    clojure.string/split-lines
    (as-> coll (map read-string coll))
    count-steps)
(-> "0
3
0
1
-3"
    clojure.string/split-lines
    (as-> coll (map read-string coll))
    count-steps)
;;11/01/2018
;;--- Day 6: Memory Reallocation ---
(defn redistribute-blocks [coll-of-colls]
  (let [current-coll (vec (last coll-of-colls))
        size (count current-coll)
        max (first (reverse (sort current-coll)))
        index (.indexOf current-coll max)]
    (loop [working-coll (assoc current-coll index 0) current-index (mod (inc index) size) blocks (get current-coll index)]
      (if (= 0 blocks)
        (conj coll-of-colls working-coll)
        (recur (update working-coll current-index inc) (mod (inc current-index) size) (dec blocks))))))
(defn cycles-while-not-repeated-config [coll]
  (loop [working-coll-of-colls (vector coll)]
    (if (not= (count working-coll-of-colls) (count (distinct working-coll-of-colls)))
      working-coll-of-colls
      (recur (redistribute-blocks working-coll-of-colls)))))
(defn skip-until-fst-to-be-repeated [coll]
  (let [el (last coll)] (drop-while #(not= % el) coll)))
(-> "banks.txt"
    slurp
    (clojure.string/split #"\s")
    (as-> c (map read-string c))
    cycles-while-not-repeated-config
    skip-until-fst-to-be-repeated
    count
    dec)
(filter #(val %) (zipmap '(0 1 2 3) '((1 2 3) (3 2 1) (2 1 3) (1 2 3))))
(let [c (zipmap '(0 1 2 3) '((1 2 3) (3 2 1) (2 1 3) (1 2 3)))] (some #{(val (first c))} (vals c)))
(filter #(some #{'(3 2 3)} '((1 2 3) (3 2 1))) '((1 2 3) (3 2 1)))
(some #{4} '(1 2 3))
(filter #('nil) '((1 2 3) (3 2 1)))
(filter #(some #{%} '((1 2 3) (3 2 1))) '((2 2 3) (3 2 1)))
(filter #(some #{(val %)} (distinct (vals {0 (1 2 3) 1 (3 2 1) 2 (1 2 3)}))) {0 (1 2 3) 1 (3 2 1) 2 (1 2 3)})
(remove nil? '((1 2 3) (3 2 1)))
(remove nil? {0 (1 2 3) 1 (3 2 1) 2 (1 2 3)})
(#{'(1 2)} (vals {0 '(1 2) 1 '(3 4)}))
(#{'(1 2)} '((1 2) (3 4)))
(find ['(1) '(2) '(3)] 2)
(drop-while #(not= % '(1)) '((3) (2) (1) (2)))
(conj [1] 2)
()
(-> [0 2 7 0]
    cycles-while-not-repeated-config
    skip-until-fst-to-be-repeated
    count
    dec)
;;12/01/2018
;;--- Day 7: Recursive Circus ---
(defn F
  ([x] x)
  ([acc el] (-> acc
                (update 0 conj (get el 0))
                (update 1 #(apply conj %1 %2) (get el 2)))))
(-> "tower.txt"
    slurp
    (clojure.string/replace " " "")
    (clojure.string/split-lines)
    (as-> s (transduce (comp
                        (map #(re-find #"([a-z]+)\(([0-9]+)\)(?:->([a-z,]+))?" %))
                        (remove #(nil? (get % 3)))
                        (map #(vec (drop 1 %)))
                        (map #(vec (remove nil? %)))
                        (map #(if (nil? (get % 2)) % (update % 2 clojure.string/split #","))))
                       F
                       [#{} #{}]
                       s))
    (as-> x (apply clojure.set/difference x))
    print)
(transduce (comp
            (map #(re-find #"([a-z]+)\(([0-9]+)\)(?:->([a-z,]+))?" %))
            (remove #(nil? (get % 3)))
            (map #(vec (drop 1 %)))
            (map #(vec (remove nil? %)))
            (map #(if (nil? (get % 2)) % (update % 2 clojure.string/split #",")))
            )
           F
           ['() '()]
           ["lwrti(72)->bli,bli"])
(def xf (comp (filter odd?) (map #(* 2 %))))
(transduce xf #(+ %1 %2) (range 5))
(#(+ %1 %2) 1 2)
(apply conj '(7 9) [1 2 3])
(conj #{} "bli")
(apply conj #{} ["bli" "blo"])
(apply clojure.set/difference [#{"bli" "blo"} #{"bli"}])
;;13/01/2018
;;--- Day 7: Recursive Circus --- Part 2
(defn find-unbalanced [t]
  (when-let [chld (:children t)]
    (do
      (if (not (:children-balanced? t))
        (let [{:keys [name children-balanced? weight-correction children]} t
              children-brief (map #(hash-map :name (:name %) :own-weight (:own-weight %) :weight (:weight %)) children)]
          (clojure.pprint/pprint {:name name :children-balanced? children-balanced? :weight-correction weight-correction :children children-brief})))
      (map #(find-unbalanced %) (:children t))
      nil)))
(defn my-distinct? [coll]
   (< 1 (count (set coll))))
(defn weigh [t name]
  (let [current (get t name)
        [_ weight children-names] current
        children-results (map #(weigh t %) (sequence children-names))
        result {:name name
                :weight weight
                :own-weight weight}]
    (if (not (empty? children-results))
      (let [children-weights (map :weight children-results)]
        (-> result
            (update :weight + (apply + children-weights))
            (assoc :children-balanced? (not (my-distinct? children-weights)))
            (as-> r (if (not (:children-balanced? r))
                      (assoc r :weight-correction (apply - (set children-weights)))
                      r))
            (assoc :children children-results)))
      result)))
(-> "tower.txt"
    slurp
    (clojure.string/split-lines)
    (as-> s (sequence (comp
                       (map #(clojure.string/replace % " " ""))
                       (map #(re-find #"([a-z]+)\(([0-9]+)\)(?:->([a-z,]+))?" %))
                       (map #(vec (drop 1 %)))
                       (map #(vec (remove nil? %)))
                       (map #(if (nil? (get % 2)) % (update % 2 clojure.string/split #",")))
                       (map #(update % 1 read-string)))
                      s))
    (as-> s (zipmap (map first s) s))
    (weigh "qibuqqg")
    clojure.pprint/pprint
    ;;find-unbalanced
    )
(sequence nil)
(sequence [1 2 3])
(not (apply distinct? '(1 1)))
(->
 "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"
 (clojure.string/split-lines)
    (as-> s (sequence (comp
                       (map #(clojure.string/replace % " " ""))
                       (map #(re-find #"([a-z]+)\(([0-9]+)\)(?:->([a-z,]+))?" %))
                       (map #(vec (drop 1 %)))
                       (map #(vec (remove nil? %)))
                       (map #(if (nil? (get % 2)) % (update % 2 clojure.string/split #",")))
                       (map #(update % 1 read-string)))
                      s))
    (as-> s (zipmap (map first s) s))
    (weigh "tknk")
    ;;clojure.pprint/pprint
    find-unbalanced
    )
(apply distinct? '(251 243 243))
(distinct? 251 243 243)
((fn my-distinct? [coll]
   (< 1 (count (set coll)))) '(243 243 243))
(count (set '(2 2 2)))
(apply - #{1 2})
(-> "king"
    slurp
    clojure.string/split-lines
    (as-> coll (filter #(clojure.string/includes? % "mobi") coll))
    (as-> coll (filter #(clojure.string/includes? % "Tower") coll))
    clojure.pprint/pprint)
(prn "=====================================================================")
;;--- Day 8: I Heard You Like Registers ---
;;14/01/2018
(defn execute
  [regs [reg op value _ logical-reg logical-op logical-value]]
  (-> regs
      (as-> registers (if ((eval (read-string logical-op)) (get registers (keyword logical-reg) 0) logical-value)
                        (assoc registers (keyword reg) ((eval (read-string op)) (get registers  (keyword reg) 0) value))
                        registers))
      (as-> registers (assoc registers :max-seen-value (apply max (vals registers))))))
(defn convert-ops [op]
  (cond
    (= op "==") "="
    (= op "!=") "not="
    (= op "inc") "+"
    (= op "dec") "-"
    :else op))
(-> "registers.txt"
    slurp
    clojure.string/split-lines
    (as-> instructions (transduce (comp
                                   (map #(clojure.string/split % #"\s"))
                                   (map #(update % 2 read-string))
                                   (map #(update % 6 read-string))
                                   (map #(update % 5 convert-ops))
                                   (map #(update % 1 convert-ops)))
                                  (completing execute)
                                  (hash-map :max-seen-value 0)
                                  instructions))
    (as-> registers (apply max (vals (dissoc registers :max-seen-value)))))
(-> "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"
    clojure.string/split-lines
    (as-> instructions (transduce (comp
                                   (map #(clojure.string/split % #"\s"))
                                   (map #(update % 2 read-string))
                                   (map #(update % 6 read-string))
                                   (map #(update % 5 convert-ops))
                                   (map #(update % 1 convert-ops)))
                                  (completing execute)
                                  (hash-map :max-seen-value 0)
                                  instructions))
    (as-> registers (apply max (vals (dissoc registers :max-seen-value)))))
((eval (read-string "=")) 1 1)
(- 0 -1)
(apply max '(1 2))
(apply max {:a 1 :b 5 :c 1})
(apply max (vals {:a 0}))
;; --- Part 2 ---
(-> "registers.txt"
    slurp
    clojure.string/split-lines
    (as-> instructions (transduce (comp
                                   (map #(clojure.string/split % #"\s"))
                                   (map #(update % 2 read-string))
                                   (map #(update % 6 read-string))
                                   (map #(update % 5 convert-ops))
                                   (map #(update % 1 convert-ops)))
                                  (completing execute)
                                  (hash-map :max-seen-value 0)
                                  instructions))
    :max-seen-value)
;;--- Day 9: Stream Processing ---
;;--Part 1--
(->  "stream.txt"
     slurp
 ;;"{}"
 ;; "{{{}}}"
 ;; "{{},{}}"
 ;; "{{{},{},{{}}}}"
 ;; "{<a>,<a>,<a>,<a>}"
 ;; "{{<ab>},{<ab>},{<ab>},{<ab>}}"
 ;; "{{<!!>},{<!!>},{<!!>},{<!!>}}"
 ;; "{{<a!>},{<a!>},{<a!>},{<ab>}}"
 ;; "{{<!>},{<!>},{<!>},{<a>}}"
 ;; "{<{},{},{{}}>}"
 ;; "<{o\"i!a,<{i<a>"
 ;; "<!!!>>"
 ;; "<!!>"
 ;; "<{!>}>"
 ;; "<<<<>"
 ;; "<random characters>"
 ;; "<>"
    (clojure.string/replace #"!." "")
    (clojure.string/replace #"<[^>]*>" "")
    (clojure.string/replace #"," "")
    (clojure.string/replace #"\n" "")
    (as-> stream (reduce add-groups {:group-value 0 :total 0} (sequence stream)))
    prn)
(clojure.string/replace "1!.!!"  #"!." "")
(clojure.string/replace "a<c>b"  #"<[^>]*>" "")
(def m (re-matcher #"<[^>]>" "<<a>>"))
(re-find m)
(re-groups (re-matcher #"<[^>]>" "<<a>>"))
(defn add-groups [state elem]
  (cond
    (= elem \{) (update state :group-value inc)
    (= elem \}) (-> state
                     (as-> s (update s :total + (get s :group-value)))
                     (update :group-value dec))
    :else state))
(reduce add-groups {:group-value 0 :total 0} (sequence "{a{a}b{}}"))
;; -- Part 2 --
(->   "stream.txt"
      slurp
 ;;"{}"
 ;; "{{{}}}"
 ;; "{{},{}}"
 ;; "{{{},{},{{}}}}"
 ;; "{<a>,<a>,<a>,<a>}"
 ;; "{{<ab>},{<ab>},{<ab>},{<ab>}}"
 ;; "{{<!!>},{<!!>},{<!!>},{<!!>}}"
 ;; "{{<a!>},{<a!>},{<a!>},{<ab>}}"
 ;; "{{<!>},{<!>},{<!>},{<a>}}"
 ;; "{<{},{},{{}}>}"
 ;; "<{o\"i!a,<{i<a>"
 ;; "<!!!>>"
 ;; "<!!>"
 ;; "<{!>}>"
 ;; "<<<<>"
 ;; "<random characters>"
 ;; "<>"
    (clojure.string/replace #"!." "")
    (as-> s (re-seq #"(<[^>]*>)" s))
    (as-> garbage-grps (map first garbage-grps))
    (as-> garbage-grps (map #(butlast (drop 1 %)) garbage-grps))
    (as-> garbage-grps (map count garbage-grps))
    (as-> garbage-grps (apply + garbage-grps))
    prn)
(let [m (re-matcher #"(<[^>]*>)" "<1><22>")]
  (re-find m)
  (re-find m)
  (re-groups m))
(re-seq #"(<[^>]*>)" "<1><22>>")
;;15/01/2018
;;--- Day 10: Knot Hash --- Part 1
(def ^:constant LIST_SIZE 256)
(defn reverse-around [coll position len]
 (let [rev (reverse (take len (drop position (into coll coll))))]
   (loop [index position working-coll coll working-rev rev]
     (if (empty? working-rev)
       working-coll
       (recur (mod (inc index) LIST_SIZE) (assoc working-coll index (first working-rev)) (rest working-rev))))))
(defn knot-hash [{:keys [position skip] :as state} len]
 (-> state
     (update :numbers reverse-around position len)
     (update :position (comp #(mod % LIST_SIZE) +) len skip)
     (update :skip inc)))
(-> "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24"
 ;;"3,4,1,5"
    (clojure.string/split #",")
    (as-> nums (transduce (comp
                           (map read-string))
                          (completing knot-hash)
                          {:numbers (vec (range LIST_SIZE)) :position 0 :skip 0}
                          nums))
    (as-> nums (apply * (take 2 (:numbers nums))))
    clojure.pprint/pprint)
;;-- Part 2
(def ^:constant LIST_SIZE 5)
(-> "49,44,50,44,51"
    (clojure.string/split #",")
    (as-> nums (transduce (comp
                           (map read-string))
                          (completing knot-hash)
                          {:numbers (vec (range LIST_SIZE)) :position 0 :skip 0}
                          nums)))
;;16/01/2018
(int \ )
(defn ascii->binary [s]
  (-> s
      sequence
      (as-> chars (transduce (comp
                              (map int)
                              (map #(str % ",")))
                             str
                             chars))))
(def ^:constant STANDARD_LENGTH_SUFFIX "17,31,73,47,23")
(def ^:constant ROUNDS 64)
(str (ascii->binary "1,2,3") STANDARD_LENGTH_SUFFIX)
(str (ascii->binary "") STANDARD_LENGTH_SUFFIX)
(defn run-round [lengths state]
  (-> lengths
      (clojure.string/split #",")
      (as-> nums (transduce (comp
                             (map read-string))
                            (completing knot-hash)
                            state
                            nums))))
(defn sparse-hash [lengths]
  (let [salted-lenghts (str (ascii->binary lengths) STANDARD_LENGTH_SUFFIX)]
      (loop [rounds-left ROUNDS state {:numbers (vec (range LIST_SIZE)) :position 0 :skip 0}]
        (if (= 0 rounds-left)
          state
          (recur (dec rounds-left) (run-round salted-lenghts state))))))
(sparse-hash "3,4,1,5")
(defn dense-hash [{:keys [numbers]}]
  (-> numbers
       (as-> nums (partition 16 nums))
       (as-> grps (transduce (comp
                         (map #(apply bit-xor %))
                         (map #(Integer/toHexString %))
                         (map #(->> %
                                    (str "0")
                                    reverse
                                    (take 2)
                                    reverse
                                    (apply str))))
                        str grps))))
(-> "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24"
 ;;""
 ;;"AoC 2017"
 ;;"1,2,3"
 ;;"1,2,4"
    sparse-hash
    dense-hash
    println)
(dense-hash '(65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22 65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22))
(Integer/toHexString 3)
(->> 40
     (str "0")
     reverse
     (take 2)
     reverse
     (apply str))
;;17/01/2018
;;--- Day 11: Hex Ed --- Part 1
(defn outline-steps [steps]
  (let [freq (frequencies steps)]
    (zipmap (map (comp keyword key) freq) (vals freq))))
(outline-steps '("n" "n" "s" "sw" "n" "s" "ne" "s" "nw" "s" "se" "n" "sw" "s"))
(group-by identity '("n" "n" "s" "sw" "n" "s" "ne" "s" "nw" "s" "se" "n" "sw" "s"))
(partition-by identity '("n" "n" "s" "sw" "n" "s" "ne" "s" "nw" "s" "se" "n" "sw" "s"))
(defn rem-opposites [steps]
  (-> steps
      (do-simplify '(:n :s) '())
      (do-simplify '(:ne :sw) '())
      (do-simplify '(:nw :se) '())))
(defn update-many [m keys f x]
  (loop [looping-m m looping-keys keys]
    (if (empty? looping-keys)
      looping-m
      (recur (update looping-m (first looping-keys) f x) (rest looping-keys)))))
(defn do-simplify [steps orig repl]
  (let [minimum (apply min (map #(get steps % 0) orig))]
    (if (>= 0 minimum)
      steps
      (-> steps
          (update-many orig (fnil - 0) minimum)
          (update-many repl (fnil + 0) minimum)))))
(defn take-shortcuts [steps]
  (-> steps
      (do-simplify '(:ne :s) '(:se))
      (do-simplify '(:nw :s) '(:sw))
      (do-simplify '(:se :n) '(:ne))
      (do-simplify '(:sw :n) '(:nw))
      (do-simplify '(:se :sw) '(:s))
      (do-simplify '(:ne :nw) '(:n))))
(defn simplify-and-cancel-steps [steps]
  (loop [looping-steps steps looping-prev-steps {}]
    (if (= looping-steps looping-prev-steps)
      looping-steps
      (recur (-> looping-steps
                 take-shortcuts
                 rem-opposites) looping-steps))))
(-> "hexed.txt"
    slurp
 ;; "ne,ne,ne"
 ;; "ne,ne,sw,sw"
 ;; "ne,ne,s,s"
 ;; "se,sw,se,sw,sw"
 ;; "se,n,n,se,sw,s,s,ne,nw,sw,nw,n"
 (clojure.string/split #",")
    outline-steps
    simplify-and-cancel-steps
    (as-> steps (transduce (map val) + steps))
    println)
;;18/01/2018
(frequencies '("n" "n" "s" "sw" "n" "s" "ne" "s" "nw" "s" "se" "n" "sw" "s"))
(group-by identity '("n" "n" "s" "sw" "n" "s" "ne" "s" "nw" "s" "se" "n" "sw" "s"))
(count (get (group-by identity '("n" "n" "s" "sw" "n" "s" "ne" "s" "nw" "s" "se" "n" "sw" "s")) "n"))
(defn simplify-steps [steps]
  (loop [looping-steps steps looping-prev-steps {}]
    (if (= looping-steps looping-prev-steps)
      looping-steps
      (recur (-> looping-steps
                 take-shortcuts) looping-steps))))
(-> "hexed.txt"
    slurp
 ;; "ne,ne,ne"
 ;; "ne,ne,sw,sw"
 ;; "ne,ne,s,s"
 ;; "se,sw,se,sw,sw"
 ;; "se,n,n,se,sw,s,s,ne,nw,sw,nw,n"
 (clojure.string/split #",")
    outline-steps
    simplify-steps
    (as-> steps (transduce (map val) + steps))
    println)
;;19/01/2018
(defn get-all-partials [coll]
 (loop [res [] looping-coll coll prev []]
       (if (empty? looping-coll)
           res
           (recur (conj res (conj prev (first looping-coll))) (rest looping-coll) (conj prev (first looping-coll))))))
(-> "hexed.txt"
    slurp
    ;;"se,n,n,se,se,sw,sw,s,s,ne,nw,sw,nw,n"
    (clojure.string/split #",")
    get-all-partials
    ;; Idea by courtesy of Dave!
    (as-> c (map #(-> %
                      outline-steps
                      simplify-and-cancel-steps
                      (as-> steps (transduce (map val) + steps))) c))
    (as-> away (apply max away))
    println)
;; --- Day 12: Digital Plumber --- Part 1
(-> "plumber.txt"
 slurp
 ;; "0 <-> 2
;; 1 <-> 1
;; 2 <-> 0, 3, 4
;; 3 <-> 2, 4
;; 4 <-> 2, 3, 6
;; 5 <-> 6
;; 6 <-> 4, 5"
 (clojure.string/split-lines)
 (as-> pipes (sequence (comp
                        (map (partial re-seq #"[0-9]+"))
                        (map #(map read-string %)))
                       pipes))
 (as-> p (zipmap (map first p) (map rest p)))
 (as-> p (find-group 0 p))
 count
 prn)
(re-seq #"([0-9]+)\s+<->\s+([0-9]+)(?:,\s+([0-9]+))*" "0 <-> 2, 3")
;;20/01/2017
(defn find-group [n conns]
  (loop [group #{}
         remaining #{n}]
    (if (empty? remaining)
      group
      (let [current (first remaining) updated-group (conj group current)]
        (recur updated-group
               (apply conj (disj remaining current) (remove updated-group (get conns current))))))))
(clojure.string/replace '("0" "2") #"\s" "")
(re-seq #"[0-9]+" "0 <-> 2, 3")
(re-find #"[0-9]+" "0 <-> 2, 3")
;; -- Part 2 --
(-> "plumber.txt"
 slurp
;;  "0 <-> 2
;; 1 <-> 1
;; 2 <-> 0, 3, 4
;; 3 <-> 2, 4
;; 4 <-> 2, 3, 6
;; 5 <-> 6
;; 6 <-> 4, 5"
 (clojure.string/split-lines)
 (as-> pipes (sequence (comp
                        (map (partial re-seq #"[0-9]+"))
                        (map #(map read-string %)))
                       pipes))
 (as-> p (zipmap (map first p) (map rest p)))
 (as-> p (for [k (keys p)]
           (find-group k p)))
 distinct
 count
 prn)
