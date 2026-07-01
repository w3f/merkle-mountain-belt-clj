(ns benchmarks
  (:require [linked-peaks :refer [play-algo algo reset-all]]
            [state]))

(defn bench-single-append
  "Measure a single append at state n. Returns {:n :time-ns :hashes}."
  [n]
  (let [cached (play-algo (dec n) false)]
    (state/reset-atoms-from-cached! cached)
    (reset! state/hash-count 0)
    (let [t0 (System/nanoTime)]
      (algo false)
      {:n n :time-ns (- (System/nanoTime) t0) :hashes @state/hash-count})))

(defn bench-append-series
  "Measure single-append time at each n, with num-trials per n."
  [n-values num-trials]
  ;; warmup
  (play-algo 100 false)
  (mapv (fn [n]
          (let [cached (play-algo (dec n) false)
                trials (doall (repeatedly num-trials
                                          (fn []
                                            (state/reset-atoms-from-cached! cached)
                                            (reset! state/hash-count 0)
                                            (let [t0 (System/nanoTime)]
                                              (algo false)
                                              {:time-ns (- (System/nanoTime) t0) :hashes @state/hash-count}))))]
            {:n n
             :mean-time-ns (/ (double (reduce + (map :time-ns trials))) num-trials)
             :mean-hashes (/ (double (reduce + (map :hashes trials))) num-trials)
             :max-hashes (apply max (map :hashes trials))}))
        n-values))

(defn bench-append-scaling
  "per-append cost across scales. one continuous incremental build; at each checkpoint n
   (ascending), time `batch` appends and report mean wall-time + hash-count stats.
   ;; hash-count per append is O(1) (<=5, n-independent); wall-time is O(log n) (node-map
   ;; depth + the belt-range-count bit scan), so it rises slowly, not constant.
   two controls: (1) a 20k-append throwaway build first, to JIT the hot path to C2 (level 4,
   https://devblogs.microsoft.com/java/how-tiered-compilation-works-in-openjdk/) before timing,
   else the first checkpoint pays compilation cost; (2) System/gc + a short settle before each
   batch, so a collection mid-batch doesn't skew the mean (uncontrolled, the fast-forward to
   large n often triggers a GC, so bigger n can measure faster: a spurious dip).
   returns rows {:n :mean-ns :mean-hashes :max-hashes}."
  [checkpoints batch]
  ;; warmup: exercise the hot path enough to trigger JIT C2, then discard
  (reset-all)
  (dotimes [_ 20000] (algo false))
  (reset-all)
  (loop [built 0
         cps (sort checkpoints)
         rows []]
    (if (empty? cps)
      rows
      (let [n (first cps)]
        ;; fast-forward (untimed) to n
        (dotimes [_ (- n built)] (algo false))
        ;; time `batch` appends individually, tracking per-append hash-count
        (let [ts (long-array batch)
              hs (long-array batch)]
          (dotimes [i batch]
            (reset! state/hash-count 0)
            (let [t0 (System/nanoTime)]
              (algo false)
              (aset ts i (- (System/nanoTime) t0))
              (aset hs i (long @state/hash-count))))
          (recur (+ n batch)
                 (rest cps)
                 (conj rows {:n n
                             :mean-ns (double (/ (reduce + (seq ts)) batch))
                             :mean-hashes (double (/ (reduce + (seq hs)) batch))
                             :max-hashes (apply max (seq hs))})))))))

(defn bench-construction
  "Measure total construction time to n leaves. Returns {:n :time-ms :total-hashes}."
  [n]
  (reset-all)
  (reset! state/hash-count 0)
  (let [t0 (System/nanoTime)]
    (doall (repeatedly n #(algo false)))
    {:n n
     :time-ms (/ (- (System/nanoTime) t0) 1e6)
     :total-hashes @state/hash-count}))

(defn bench-construction-series [n-values]
  ;; warmup
  (play-algo 100 false)
  (mapv bench-construction n-values))

(defn spit-csv [rows file]
  (let [ks (keys (first rows))
        header (clojure.string/join "," (map name ks))]
    (spit file (str header "\n"
                    (clojure.string/join "\n"
                                         (map #(clojure.string/join "," (map % ks)) rows))
                    "\n"))))

(comment
  ;; TODO: add rand gen specifying couple orders of magnitude
  ;; cache past runs
  (bench-append-series [100 101 500 501 1000 1001 2000 5000 10000] 10)
  (bench-construction-series [100 500 1000 2000 5000 10000])
  (spit-csv (bench-append-series [100 500 1000 2000 5000] 10) "stats/append-time.csv")
  (spit-csv (bench-construction-series [100 500 1000 2000 5000]) "stats/construction-time.csv")
 ;; constant-time append across orders of magnitude (10^6 checkpoint takes a couple minutes to build):
  (bench-append-scaling [1000 10000 100000 1000000] 500)
  (spit-csv (bench-append-scaling [1000 10000 100000] 500) "stats/append-scaling.csv"))
