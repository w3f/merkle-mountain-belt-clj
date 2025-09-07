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

(defn spit-lazy-to-file [lazy file append]
  (spit file (with-out-str (doseq [i (doall lazy)] (println i))) :append append))

(spit-lazy-to-file
 (pmap (fn [k] {(inc k) (float (avg (map #(proof-size % k)
                                        (apply range (let [base (Math/pow 2 24)] [base (+ base (Math/pow 2 20))])))))})
       (map dec [1 10 50 200 600]))
 "stats/absolute-proof-size.dat" false)

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

(every? true? (map (fn [m] (let [n 100
                                expected-proof-size (proof-size n (- n m))
                                actual-proof-size (count (proof/co-path-internal (primitives.storage/leaf-location m) [] nil true))]
                            (= expected-proof-size actual-proof-size)))
                   (range 1 101)))

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
                                                                                                           (float)
                                                                                                           ))
                                                                                    }
                                                                          )
                                                                        (range n-range-min n-range-max step-size)))]
                                                   (print i "\n"))) :append false))

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
