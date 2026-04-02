(ns proof-size
  (:require [primitives.core :as p]
            [primitives.storage]
            [primitives.proof :as proof]
            [clojure.test]
            [state :refer [leaf-count]]
            [linked-peaks :refer [algo]]))

(defn range-splits [S-n]
  (first (reduce (fn [[acc last-elem but-last-elem] elem]
                   [(if (or (= last-elem but-last-elem) (= 2 (- last-elem elem)))
                      (conj acc [elem])
                      (conj (or (pop acc) []) (conj (last acc) elem)))
                    elem last-elem])
                 [[[]] 0 -1]
                 S-n)))

(defn range-position-flat [m S-n acc-leaves current-range]
  (let [last-range-max-m (+ acc-leaves (Math/pow 2 (last S-n)))]
    (if (< m last-range-max-m)
      current-range
      (range-position-flat m (butlast S-n) last-range-max-m (inc current-range)))))

(defn nth-reverse [coll n]
  (nth coll (- (dec (count coll)) n)))

(defn range-position-nested [m S-n]
  (let [range-position (range-position-flat m S-n 0 0)
        range-splits (range-splits S-n)
        ]
    (letfn [(range-position-nested-internal [range-position range-splits acc-ranges]
              (let [last-range-length (count (peek range-splits))]
                (if (< range-position last-range-length)
                  [acc-ranges range-position]
                  (range-position-nested-internal (- range-position last-range-length) (pop range-splits) (inc acc-ranges))))
              )]
      (range-position-nested-internal range-position range-splits 0)
      )
    ))

(defn avg [coll]
  (/ (reduce + coll) (count coll)))

(defn proof-size
  "returns the proof size for an MMB size `n` with leaf depth `k`"
 [n k]
   (let [S-n (p/S-n n)
         range-splits (range-splits S-n)
         range-position-flat (range-position-flat k S-n 0 0)
         range-position-nested (range-position-nested k S-n)
         peak-height (nth-reverse S-n range-position-flat)
         aggregate-for-left-range-nodes (if (< (second range-position-nested)
                                               (dec (count (nth-reverse range-splits (first range-position-nested)))))
                                          1 0)
         aggregate-for-left-belt-nodes (if (< (first range-position-nested) (dec (count range-splits)))
                                         1 0)
         ]
     ;; {:splits range-splits
     ;;  :height peak-height
     ;;  :r range-position-nested
     ;;  :ar aggregate-for-left-range-nodes
     ;;  :al aggregate-for-left-belt-nodes
     ;;  :agg (apply + (flatten [peak-height range-position-nested aggregate-for-left-range-nodes aggregate-for-left-belt-nodes]))}
   (apply + (flatten [peak-height range-position-nested aggregate-for-left-range-nodes aggregate-for-left-belt-nodes]))))

(comment (float (/ (* (+ 2 (* (dec 100))) 2500) 7886898)))

(comment (pmap (fn [k] {(inc k) (float (avg (map #(proof-size % k)
                                                (apply range (let [base (Math/pow 2 24)] [base (+ base (Math/pow 2 20))])))))})
               (map dec [1 10 50 200 600])))

(defn spit-lazy-to-file [lazy file append]
  (spit file (with-out-str (doseq [i (doall lazy)] (println i))) :append append))

(comment
  (spit-lazy-to-file
   (pmap (fn [k] {(inc k) (float (avg (map #(proof-size % k)
                                           (apply range (let [base (Math/pow 2 24)] [base (+ base (Math/pow 2 20))])))))})
         (map dec [1 10 50 200 600]))
   "stats/absolute-proof-size.dat" false))

;; loop for checking that proof-size calculation matches implementation
(comment
  (doall (repeatedly 150 #(algo false)))
  ;; append to file
  (doall (repeatedly 2000 #(do (algo false)
                               (spit "stats/leaf-count.dat" (str  [@leaf-count (every? true? (pmap (fn [m] (let [n @leaf-count
                                                                                                          expected-proof-size (proof-size n (- n m))
                                                                                                          actual-proof-size (count (proof/co-path-internal (primitives.storage/leaf-location m) [] nil true))]
                                                                                                      (= expected-proof-size actual-proof-size)))
                                                                                             (range 1 151)))]) "append" true)
                               ))))

(comment
  (every? true? (map (fn [m] (let [n 100
                                   expected-proof-size (proof-size n (- n m))
                                   actual-proof-size (count (proof/co-path-internal (primitives.storage/leaf-location m) [] nil true))]
                               (= expected-proof-size actual-proof-size)))
                     (range 1 101))))


(comment
  (let [blocks-per-minute 10
        blocks-per-year (* blocks-per-minute 60 24 365)
        n-range-min (* 2 blocks-per-year)
        n-range-max (+ n-range-min (* 5 blocks-per-year))
        n-max (Math/pow 2 11)
        k 2400
        step-size 1000000]
    (spit (str "stats/avg-proof-size-" k ".dat") (with-out-str (doseq [i (doall (pmap (fn [n-min] {:n-min n-min
                                                                                      :avg-proof-sizes (let [n-max (+ n-min n-max)]
                                                                                                         (-> (->> (map (fn [n] (map (fn [m] (proof-size n m))
                                                                                                                                   (range 0 k))) (range n-min n-max))
                                                                                                                  (flatten)
                                                                                                                  (reduce +))
                                                                                                             (/ (* k (- n-max n-min)))
                                                                                                             (float)))})
                                                                          (range n-range-min n-range-max step-size)))]
                                                     (print i "\n"))) :append false)))

(comment
  (spit "avg-proof-size.dat" (str (doall (map inc (doall (range 0 10))))))

  (let [realized-sequence (doall (map inc (doall (range 0 10))))]
    (spit "avg-proof-size.dat" (str realized-sequence)))

  (spit "avg-proof-size.dat"
        (with-out-str (doseq [i (map inc (doall (range 0 10)))] (print i "\n")))))
;; -> cached eval
(comment {:n-min 15768000, :avg-proof-sizes '(4.9596 7.84344 9.960587 11.3155365 12.626498 15.109105)})

;; -> cached eval
(comment {:n-min 10512000, :avg-proof-size 9.951567} {:n-min 11012000, :avg-proof-size 9.954033} {:n-min 11512000, :avg-proof-size 9.961166} {:n-min 12012000, :avg-proof-size 9.95232}
         {:n-min 12512000, :avg-proof-size 9.940086} {:n-min 13012000, :avg-proof-size 9.94288} {:n-min 13512000, :avg-proof-size 9.95522} {:n-min 14012000, :avg-proof-size 9.952746}
         {:n-min 14512000, :avg-proof-size 9.951567} {:n-min 15012000, :avg-proof-size 9.954033} {:n-min 15512000, :avg-proof-size 9.960587} {:n-min 16012000, :avg-proof-size 9.96296}
         {:n-min 16512000, :avg-proof-size 9.9345665} {:n-min 17012000, :avg-proof-size 9.94358} {:n-min 17512000, :avg-proof-size 9.95522} {:n-min 18012000, :avg-proof-size 9.952746}
         {:n-min 18512000, :avg-proof-size 9.951567} {:n-min 19012000, :avg-proof-size 9.954733} {:n-min 19512000, :avg-proof-size 9.961166} {:n-min 20012000, :avg-proof-size 9.95232}
         {:n-min 20512000, :avg-proof-size 9.940086} {:n-min 21012000, :avg-proof-size 9.94288} {:n-min 21512000, :avg-proof-size 9.95522} {:n-min 22012000, :avg-proof-size 9.952746}
         {:n-min 22512000, :avg-proof-size 9.95172} {:n-min 23012000, :avg-proof-size 9.954033} {:n-min 23512000, :avg-proof-size 9.960587} {:n-min 24012000, :avg-proof-size 9.96296}
         {:n-min 24512000, :avg-proof-size 9.9345665} {:n-min 25012000, :avg-proof-size 9.94358} {:n-min 25512000, :avg-proof-size 9.95522} {:n-min 26012000, :avg-proof-size 9.952746}
         {:n-min 26512000, :avg-proof-size 9.951567} {:n-min 27012000, :avg-proof-size 9.954033} {:n-min 27512000, :avg-proof-size 9.961166} {:n-min 28012000, :avg-proof-size 9.95232}
         {:n-min 28512000, :avg-proof-size 9.940086} {:n-min 29012000, :avg-proof-size 9.94288} {:n-min 29512000, :avg-proof-size 9.95522} {:n-min 30012000, :avg-proof-size 9.952746}
         {:n-min 30512000, :avg-proof-size 9.951567} {:n-min 31012000, :avg-proof-size 9.954033} {:n-min 31512000, :avg-proof-size 9.960587} {:n-min 32012000, :avg-proof-size 9.96296}
         {:n-min 32512000, :avg-proof-size 9.9345665} {:n-min 33012000, :avg-proof-size 9.94358} {:n-min 33512000, :avg-proof-size 9.95522} {:n-min 34012000, :avg-proof-size 9.952746}
         {:n-min 34512000, :avg-proof-size 9.951567} {:n-min 35012000, :avg-proof-size 9.954733} {:n-min 35512000, :avg-proof-size 9.961166} {:n-min 36012000, :avg-proof-size 9.95232}
         {:n-min 36512000, :avg-proof-size 9.940086})

(do (clojure.test/deftest test-range-splits
      (clojure.test/is
       (every? true? (map (fn [n m] (let [S-n (p/S-n n)
                                         range-splits (range-splits S-n)
                                         range-position-flat (range-position-flat m S-n 0 0)
                                         range-position-nested (range-position-nested m S-n)
                                         surrounding-range (nth range-splits (- (dec (count range-splits)) (first range-position-nested)))]
                                     ;; [range-splits
                                     ;;  {:pos range-position-flat :height (nth S-n (- (dec (count S-n)) range-position-flat))}
                                     ;;  {:pos range-position-nested :height (nth surrounding-range (- (dec (count surrounding-range)) (second range-position-nested)))}
                                     ;;  ]
                                     (=
                                      (nth S-n (- (dec (count S-n)) range-position-flat))
                                      (nth (nth range-splits (- (dec (count range-splits)) (first range-position-nested)))
                                           (- (dec (count surrounding-range)) (second range-position-nested))))
                                     ))
                          (take 200000 (repeat 20000000)) (range 1 200000)))
       ))
    (clojure.test/run-test test-range-splits))

(let [n 2000
      S-n (p/S-n n)
      range-splits (range-splits S-n)]
  [(println (into [] S-n))
   (println (first range-splits))
   (= S-n (flatten range-splits))])
