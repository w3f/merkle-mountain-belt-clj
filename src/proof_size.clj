(ns proof-size
  (:require [primitives.core :as p]
            [primitives.storage]
            [primitives.proof :as proof]
            [clojure.test]
            [state :refer [leaf-count]]
            [linked-peaks :refer [algo reset-all]]))

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

(defn proof-size [n m]
   (let [S-n (p/S-n n)
         range-splits (range-splits S-n)
         range-position-flat (range-position-flat m S-n 0 0)
         range-position-nested (range-position-nested m S-n)
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


;; loop for checking that proof-size calculation matches implementation
(comment
  (doall (repeatedly 150 #(algo false)))
  ;; append to file
  (doall (repeatedly 2000 #(do (algo false)
                               (spit "leaf-count.dat" (str  [@leaf-count (every? true? (pmap (fn [m] (let [n @leaf-count
                                                                                                          expected-proof-size (proof-size n (- n m))
                                                                                                          actual-proof-size (count (proof/co-path-internal (primitives.storage/leaf-location m) [] nil true))]
                                                                                                      (= expected-proof-size actual-proof-size)))
                                                                                             (range 1 151)))]) :append true)
                               ))))

(every? true? (map (fn [m] (let [n 100
                                expected-proof-size (proof-size n (- n m))
                                actual-proof-size (count (proof/co-path-internal (primitives.storage/leaf-location m) [] nil true))]
                            (= expected-proof-size actual-proof-size)))
                   (range 1 101)))

(map (fn [n-min] (let [m (* 15 10)
                      n-max (+ n-min 1000)]
                  (-> (->> (pmap (fn [n] (map (fn [m] (proof-size n m))
                                             (range 0 m))) (range n-min n-max))
                           (flatten)
                           (reduce +))
                      (/ (* m (- n-max n-min)))
                      (float)
                      )))
     ;; (range 1000000 30000000 1000000)
     (range 1500000 1500001))

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
