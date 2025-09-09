(ns k-vs-n
  (:require [linked-peaks]
            [primitives.proof]
            [primitives.storage]
            [primitives.core :refer [S-n belt-ranges-lengths]]
            [clojure.string]
            ;; [state :refer [node-array node-map]]
            ))

;; (defn proof-size [n k]
;;   (count (primitives.proof/co-path-internal (primitives.storage/leaf-location (- n k)) [] nil true)))

(let [max-n 100]
  (def results (atom []))
  (linked-peaks/reset-all)
  (doseq [n (range 1 (inc max-n))]
    (linked-peaks/algo false)
    (swap! results conj (doall (map
                                (fn [k] (count (primitives.proof/co-path-internal (primitives.storage/leaf-location (- n k)) [] nil true)))
                                (range 0 n)))))
  @results)

(let [n 1337]
  (def result-singleton (atom nil))
  (linked-peaks/reset-all)
  (linked-peaks/play-algo n true)
  nil
  ;; (reset! result-singleton (map (fn [k] (count (primitives.proof/co-path-internal (primitives.storage/leaf-location (- n k)) [] nil true))) (range 0 n)))
  )

(let [n 1337] (reset! result-singleton (map (fn [k] (count (primitives.proof/co-path-internal (primitives.storage/leaf-location (- n k)) [] nil true))) (range 0 1337))) nil)

(let [max-n 10
      values (nth @results (dec max-n))
      ;; Get list for n (0-based index) padded
      padded (concat values (repeat (- max-n (count values)) 0))
      rows (clojure.string/join "," padded)]

  ;; Write CSV content to file (no header)
  (spit "table.csv" (clojure.string/join "\n" rows)))

(def data (for [n (range 1 (inc max-n))]
            (do (linked-peaks/play-algo-oneshot-end n)
                (map
                 (fn [k] [[n k] (count (primitives.proof/co-path-internal (primitives.storage/leaf-location (- n k)) [] nil true))])
                 (range 0 n)))))

(do (linked-peaks/play-algo-oneshot-end 5)
    [(primitives.proof/co-path-internal (primitives.storage/leaf-location 5) [] nil true)
     (count (primitives.proof/co-path-internal (primitives.storage/leaf-location 5) [] nil true))])

(every? (fn [[a b]] (= a b)) (for [n (range 1 (inc 1000))]
                               [(count (primitives.core/S-n n)) (apply + (primitives.core/belt-ranges-lengths n))]))

(reverse (primitives.core/S-n 5))
(reverse (primitives.core/belt-ranges-lengths 5))

(let [n 1337
      peak-n 0
      peaks (reverse (S-n n))
      bagging (reverse (belt-ranges-lengths n))
      ;; ---
      peak-proof (nth peaks peak-n)
      ;; range-proof
      ]
  [peaks bagging]
  ;; [peak-proof ]
  )

(defn- bag-peaks-to-ranges-inner [acc peak-remainder bagging-remainder]
  (if (empty? peak-remainder)
    acc
    (let [bagging-current (first bagging-remainder)
          bagging-rest (rest bagging-remainder)
          peak-split (split-at bagging-current peak-remainder)]
      (bag-peaks-to-ranges-inner (concat acc [(first peak-split)]) (second peak-split) bagging-rest))))

(bag-peaks-to-ranges-inner [] (reverse (S-n 1337)) (reverse (belt-ranges-lengths 1337)))
(bag-peaks-to-ranges-inner [] (S-n 1337) (belt-ranges-lengths 1337))

(defn bag-peaks-to-ranges [n]
  (bag-peaks-to-ranges-inner [] (S-n n) (belt-ranges-lengths n)))

(defn- peak-position-inner [peak-n acc acc-peaks range-remainder]
  (let [peak-sum (+ acc (first range-remainder))]
    (if (< peak-sum peak-n)
      (peak-position-inner peak-n peak-sum (inc acc-peaks) (rest range-remainder))
      {:belt-position acc-peaks
       :range-length (first range-remainder)
       :range-position (- (dec (first range-remainder)) (- peak-sum peak-n))})))

(defn peak-position [n peak-n]
  (let [ranges (belt-ranges-lengths n)]
    (merge {:belt-length (count ranges)} (peak-position-inner peak-n 0 0 (reverse ranges)))))

(let [n 1337
      peak-n 1]
  [(bag-peaks-to-ranges n) (peak-position n peak-n)])

;; attempt: get bagging representation from n
(let [n 1337
      peaks (reverse (S-n n))
      peak-n (- (count peaks) 0)
      ranges (reverse (belt-ranges-lengths n))
      bagging (bag-peaks-to-ranges n)
      ;; ---
      ;; range-proof
      ]
  [peaks ranges (reverse bagging)]
  ;; [peak-proof ]
  )

;; proof length for every peak's leaf
;; want to segregate this into
;; 1. component up until peak (just height of peak)
;; 2. proof component within range until tip of range
;; 3. proof component from tip of range to belt root
;; let's try starting from the last peak, and going
;; backwards recursively:
(defn proof-lengths-for-peak [n, peak-n, include-phantom?]
  (let [bagging (bag-peaks-to-ranges n)
        peak-position (peak-position n (inc peak-n))
        peak-component (nth (reverse (S-n n)) peak-n)
        range-component ()]
    (if include-phantom?
      ;; TODO
      [[(reverse (map reverse bagging)) peak-position]
       {:peak-component peak-component
        ;; if we have phantom (range) nodes, we always add another proof item to the position within the range.
        :range-component (inc (:range-position peak-position))
        ;; if we have phantom (belt) nodes, we always add another proof item to the position within the range.
        :belt-component (inc (:belt-position peak-position))}]
      [[(reverse (map reverse bagging)) peak-position]
       {:peak-component peak-component
        ;; if the peak is at the left edge of a range (always the case for singleton ranges), without phantom (range) nodes, we have no left item in the range path, hence the range component is simply the position of the peak in the range. Else, add another proof item to the position within the range.
        :range-component (if (= (:range-position peak-position) (dec (:range-length peak-position)))
                           (:range-position peak-position)
                           (inc (:range-position peak-position)))
        ;; if the peak's range is at the left edge of the belt (always the case for singleton belt), without phantom (belt) nodes, we have no left item in the belt path, hence the belt component is simply the position of the peak's range in the belt (0 for singleton belt). Else, add another proof item to the position within the belt.
        :belt-component (if (= (:belt-position peak-position) (dec (:belt-length peak-position)))
                          (:belt-position peak-position)
                          (inc (:belt-position peak-position)))}])))

(defn proof-length-for-peak [n, peak-n, include-phantom?]
  [[(reverse (map reverse (bag-peaks-to-ranges n))) (peak-position n (inc peak-n))]
   (apply + (vals (last (proof-lengths-for-peak n, peak-n, include-phantom?))))])

(proof-lengths-for-peak 1337 0 false)
(proof-length-for-peak 1337 3 false)

(last (proof-length-for-peak 2 0 false))

(let [n 10
      k 10
      peaks (reverse (S-n n))
      peak-max-leaf-mapping (rest (reduce (fn [acc i] (conj acc (+ (last acc) i))) [0] (map #(int (Math/pow % 2)) peaks)))
      location (count (take-while #(<= % (dec k)) peak-max-leaf-mapping))]
  [peaks peak-max-leaf-mapping location])

(defn k-to-peak [k n]
  (let [peaks (reverse (S-n n))
        peak-max-leaf-mapping (rest (reduce (fn [acc i] (conj acc (+ (last acc) i))) [0] (map #(int (Math/pow 2 %)) peaks)))
        location (count (take-while #(<= % k) peak-max-leaf-mapping))]
    [peaks peak-max-leaf-mapping location]))

(defn proof-length-for-leaf [n k include-phantom?]
  (last (proof-length-for-peak n (last (k-to-peak k n)) include-phantom?)))

(defn proof-lengths-for-n-inefficient [n include-phantom?]
  (let [proof-lengths-peaks (map #(last (proof-length-for-peak n % include-phantom?)) (range 0 (count (S-n n))))]
    (map #(nth proof-lengths-peaks (last (k-to-peak % n))) (range 0 n))))

(defn proof-lengths-for-n [n include-phantom?]
  (let [proof-lengths-peaks (map #(last (proof-length-for-peak n % include-phantom?)) (range 0 (count (S-n n))))
        peak-heights (reverse (S-n n))]
    ;; (map #(nth proof-lengths-peaks (last (k-to-peak % n))) (range 0 n))
    ;; [proof-lengths-peaks peak-heights]
    (reduce (fn [acc k] (concat acc (repeat (Math/pow 2 (nth peak-heights k)) (nth proof-lengths-peaks k)))) [] (range (count proof-lengths-peaks)))))

(proof-length-for-leaf 3 1 false)
(k-to-peak 3 3)

(let [n 20
      k 20]
  [(bag-peaks-to-ranges n) (proof-length-for-leaf n k false)])

(let [n 20] (map #(last (k-to-peak % n)) (range 0 n)))

(let [n 5] [(map (fn [n]
                   (map (fn [peak-n]
                          (last (proof-length-for-peak n peak-n false)))
                        (range 0 (count (S-n n)))))
                 (range 1 (inc n)))
            (take n @results)])

(every? true? (map (fn [n] (apply = [(map (fn [n]
                                            (map (fn [k]
                                                   (proof-length-for-leaf n k false))
                                                 (range 0 n)))
                                          (range 1 (inc n)))
                                     (take n @results)]))
                   (range 1 100)))

(every? true? (map (fn [n] (apply = [(map (fn [n]
                                            (proof-lengths-for-n n false))
                                          (range 1 (inc n)))
                                     (take n @results)]))
                   (range 1 100)))

(map (fn [n]
       (map (fn [k]
              (proof-length-for-leaf n k false))
            (range 0 n)))
     (range 1 (inc 1000)))

(let [max-n 16384
      values (map (fn [n]
                    (proof-lengths-for-n n false))
                  (range 1 (inc max-n)))
      padded (map #(concat % (repeat (- max-n (count %)) 0)) values)
      rows (map #(clojure.string/join "," %) padded)]

  (spit "stats/k-vs-n-no-phantom.csv" (clojure.string/join "\n" rows)))

(let [max-n 16384
      values (map (fn [n]
                    (proof-lengths-for-n n true))
                  (range 1 (inc max-n)))
      padded (map #(concat % (repeat (- max-n (count %)) 0)) values)
      rows (map #(clojure.string/join "," %) padded)]

  (spit "stats/k-vs-n-with-phantom.csv" (clojure.string/join "\n" rows)))

(proof-length-for-leaf 3 2 false)
(belt-ranges-lengths 3)
