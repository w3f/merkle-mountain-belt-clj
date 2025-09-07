(ns k-vs-n
  (:require [linked-peaks]
            [primitives.proof]
            [primitives.storage]
            [primitives.core :refer [S-n belt-ranges-lengths]]
            ;; [state :refer [node-array node-map]]
            ))

(let [max-n 50]
  (def results (atom []))
  (linked-peaks/reset-all)
  (doseq [n (range 1 (inc max-n))]
    (linked-peaks/algo false)
    (swap! results conj (doall (map
                                (fn [k] (count (primitives.proof/co-path-internal (primitives.storage/leaf-location (- n k)) [] nil true)))
                                (range 0 n))))))

(let [max-n 10
      values (nth @results (dec n))
      ;; Get list for n (0-based index) padded
      padded (concat values (repeat (- max-n (count values)) 0))
      rows (clojure.string/join "," padded)]

  ;; Write CSV content to file (no header)
  (spit "table.csv" (clojure.string/join "\n" rows)))

(println @results)

(def max-n 20)
(def data (for [n (range 1 (inc max-n))]
            (do (linked-peaks/play-algo-oneshot-end n)
                (map
                 (fn [k] [[n k] (count (primitives.proof/co-path-internal (primitives.storage/leaf-location (- n k)) [] nil true))])
                 (range 0 n)))))
(println data)

(do (linked-peaks/play-algo-oneshot-end 5)
    [(primitives.proof/co-path-internal (primitives.storage/leaf-location 5) [] nil true)
     (count (primitives.proof/co-path-internal (primitives.storage/leaf-location 5) [] nil true))])

(let [max-n 5])

;; Output as a Markdown table

;; do this more efficiently
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
      [acc-peaks (- (dec (first range-remainder)) (- peak-sum peak-n))])))

(defn peak-position [n peak-n]
  (let [ranges (belt-ranges-lengths n)]
    (peak-position-inner peak-n 0 0 (reverse ranges))))

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
(defn proof-lengths-for-peak [n, peak-n]
  (let [peaks (S-n n)]))

(primitives.core/belt-ranges-lengths 5)
(primitives.core/S-n 5)
