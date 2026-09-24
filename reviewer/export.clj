;; Run from the repository root: clojure -M reviewer/export.clj
;; No additional dependencies. The generated page contains no source URLs or Git metadata.
(ns reviewer.export
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]
            [hashing :as h]
            [linked-peaks :as lp]
            [paper-test :as paper]
            [primitives.core :as p]
            [proof-size :as ps]
            [state :as state]))

(def max-n 64)
(def reference-limit 100000)

(defn ensure! [ok message data]
  (when-not ok (throw (ex-info message data))))

(defn json [x]
  (cond
    (nil? x) "null"
    (string? x) (-> (pr-str x) (str/replace "<" "\\u003c")
                    (str/replace "\u2028" "\\u2028") (str/replace "\u2029" "\\u2029"))
    (keyword? x) (json (name x))
    (boolean? x) (str x)
    (number? x) (str (if (ratio? x) (double x) x))
    (map? x) (str "{" (str/join "," (for [[k v] (sort-by (comp str key) x)]
                                      (str (json (name k)) ":" (json v)))) "}")
    (sequential? x) (str "[" (str/join "," (map json x)) "]")
    :else (throw (ex-info "Unsupported JSON value" {:value x}))))

(defn structural-state [n]
  (let [snapshot (state/current-atom-states)
        [vertices links] (lp/nodes-edges true true)
        nodes (mapv (fn [{:keys [id pos]}]
                      (let [[span type] (str/split id #":")
                            [lo hi] (edn/read-string span)
                            [x y] (map #(Double/parseDouble %) (str/split (str/replace pos "!" "") #","))]
                        {:id id :lo lo :hi hi :type (if (= lo hi) "leaf" type) :x x :y y}))
                    (sort-by :id vertices))
        edges (vec (sort (remove (fn [[a b]] (= a b)) links)))
        node-by-id (into {} (map (juxt :id identity) nodes))
        children (group-by first edges)
        child-ids (set (map second edges))
        roots (remove child-ids (map :id nodes))
        digests (atom {})]
    (ensure! (= 1 (count roots)) "The exported graph must have one root" {:n n :roots roots})
    (letfn [(digest [id]
              (or (get @digests id)
                  (let [node (get node-by-id id)
                        cs (sort-by #(:lo (get node-by-id %)) (map second (get children id)))
                        value (h/with-backend :keccak
                                (if (= "leaf" (:type node))
                                  (h/leaf-hash (:lo node))
                                  (do (ensure! (= 2 (count cs)) "Non-leaf must have two children" {:n n :id id :children cs})
                                      (h/node-hash (digest (first cs)) (digest (second cs))))))]
                    (swap! digests assoc id value)
                    value)))]
      (let [root (digest (first roots))
            observed (vec (rest (reverse (map :height (paper/peaks-from-state snapshot (:rightmostP snapshot) [])))))
            proofs (mapv (fn [leaf]
                           (let [proof (lp/membership-proof-leaf leaf snapshot)
                                 expected (ps/proof-size n (paper/k-from-leaf n leaf))]
                             (ensure! (lp/verify-membership proof (:root-belt-node snapshot)) "Existing interval proof failed" {:n n :leaf leaf})
                             (ensure! (= expected (count (:co-path proof))) "Proof size differs from paper formula" {:n n :leaf leaf})
                             {:leaf leaf :siblings (vec (:co-path proof)) :expectedSize expected}))
                         (range 1 (inc n)))]
        {:n n :nodes (mapv #(assoc % :digest (digest (:id %))) nodes) :edges edges
         :graphRoot root :peaks observed :expectedPeaks (vec (p/S-n n))
         :ranges (mapv vec (ps/range-splits observed)) :proofs proofs}))))

(defn structural-traces []
  (h/with-backend :interval
    (lp/reset-all)
    (mapv (fn [n] (lp/algo false) (structural-state n)) (range 1 (inc max-n)))))

(defn reduce-keccak-traces [limit consume initial]
  (h/with-backend :keccak
    (lp/reset-all)
    (let [original lp/hash-union]
      (reduce (fn [result n]
              (let [events (atom [])]
                (reset! state/hash-count 0)
                ;; Observe the counted construction wrapper, preserving its behavior.
                ;; Verification/reuse checks use raw-hash-union and are not counted work.
                (with-redefs [lp/hash-union
                              (fn [& args]
                                (let [result (apply original args)]
                                  (when (and (= 2 (count args)) (apply h/real-union? args))
                                    (swap! events conj {:left (first args) :right (second args) :result result}))
                                  result))]
                  (lp/algo false))
                (ensure! (= @state/hash-count (count @events)) "Trace differs from implementation counter" {:n n})
                (consume result
                         {:n n :root @state/root-belt-node :leafHash (h/leaf-hash n)
                          :hashes @state/hash-count :events @events
                          :case (cond (= (inc n) (Long/highestOneBit (inc n))) "no-merge"
                                      (even? n) "fresh" :else "delayed")})))
              initial (range 1 (inc limit))))))

(defn keccak-traces []
  (reduce-keccak-traces max-n conj []))

(defn amortized-samples []
  (mapv (fn [k]
          (let [d (int (Math/floor (/ (Math/log (inc k)) (Math/log 2))))
                period (bit-shift-left 1 (inc d))
                sizes (paper/empirical-mmb-proof-sizes k period)]
            {:k k :period period :sizes sizes
             :ummbObserved (paper/amortized-structural paper/ummb-proof-size k 1)
             :ummbFormula (paper/amortized-ummb-lemma k)
             :restricted (paper/amortized-structural-restricted paper/ummb-proof-size k)
             :mmbObserved (/ (double (reduce + sizes)) period)
             :mmbStructural (paper/amortized-structural ps/proof-size k 1)
             :mmbBound (paper/amortized-mmb-upper-bound k)}))
        (range 1 17)))

(def test-symbols
  '[paper-figures-test lemma-16-test lemma-17-hash-count-test
    membership-proofs-test membership-proofs-large-test])

(defn source-test-results []
  (mapv (fn [sym]
          (println "Running existing paper test:" sym)
          (binding [test/*report-counters* (ref test/*initial-report-counters*)
                    test/*test-out* (java.io.StringWriter.)]
            (test/test-vars [(ns-resolve 'paper-test sym)])
            (let [counts @test/*report-counters*]
              (ensure! (zero? (+ (:fail counts) (:error counts))) "Existing paper test failed" {:test sym :counts counts})
              {:name (str sym) :assertions (:pass counts) :passed true})))
        test-symbols))

(defn live-reference []
  (let [checkpoints (set (concat [1337 50000 99999 reference-limit]
                                (for [power (range 6 17) delta [-1 0 1]
                                      :let [n (+ (bit-shift-left 1 power) delta)]
                                      :when (< max-n n (inc reference-limit))] n)))
        roots (StringBuilder. (* 64 reference-limit))
        ;; Keep one root and count per prefix, not a full tree or event history.
        reference (reduce-keccak-traces
                   reference-limit
                   (fn [result {:keys [n root hashes] :as trace}]
                     (ensure! (re-matches #"[0-9a-f]{64}" root) "Invalid reference root" {:n n})
                     (.append roots ^String root)
                     (when (zero? (mod n 10000))
                       (println "Clojure reference construction:" n "/" reference-limit))
                     (cond-> (update result :hashCounts conj hashes)
                       (contains? checkpoints n) (update :checkpoints conj trace)))
                   {:maxN reference-limit :hashCounts [] :checkpoints []})]
    (assoc reference
     ;; Root for n occupies [(n-1)*64, n*64); fixed-width hex avoids JSON array overhead.
     :rootsHex (str roots)
     :hashVectors (mapv (fn [length]
                         (let [bytes (byte-array (map unchecked-byte (range length)))]
                           {:input (h/->hex bytes) :digest (h/->hex (h/keccak256 bytes))}))
                       [0 1 8 64 135 136 137 272]))))

(defn build! []
  (println "Exporting" max-n "incremental states with independent Keccak graph checks")
  (let [structures (structural-traces)
        crypto (keccak-traces)
        expected-counts (vec (paper/hash-counts-per-append max-n))
        states (mapv (fn [structure actual]
                       (ensure! (= (:graphRoot structure) (:root actual)) "Graph topology disagrees with Keccak commitment" {:n (:n actual)})
                       (ensure! (= (:peaks structure) (:expectedPeaks structure)) "Peak schedule mismatch" {:n (:n actual)})
                       (ensure! (= (:hashes actual) (nth expected-counts (dec (:n actual)))) "Counter disagrees with paper-test" {:n (:n actual)})
                       (merge (dissoc structure :graphRoot) (dissoc actual :n)))
                     structures crypto)
        data {:formatVersion 3 :maxN max-n :states states :amortized (amortized-samples)
              :liveReference (live-reference)
              :sourceTests (source-test-results)
              :scope "Clojure reference fixtures for the independent browser calculator. Source test results are recorded at export; they are not browser executions of the JVM suite."}
        template (slurp "reviewer/explorer.html")
        html (-> template
                 (str/replace "/*__STYLES__*/" (slurp "reviewer/explorer.css"))
                 (str/replace "/*__LIVE__*/" (slurp "reviewer/live.js"))
                 (str/replace "/*__STRUCTURE__*/" (slurp "reviewer/structure.js"))
                 (str/replace "/*__APP__*/" (slurp "reviewer/explorer.js"))
                 (str/replace "/*__DATA__*/" (json data)))]
    (ensure! (not (re-find #"/\*__(?:STYLES|LIVE|STRUCTURE|APP|DATA)__\*/" html)) "Unfilled template" {})
    (io/make-parents "docs/index.html")
    (spit "docs/index.html" html)
    (spit "docs/.nojekyll" "")
    (println "Wrote docs/index.html:" (count (.getBytes html "UTF-8")) "bytes;" (count states) "states; all export checks passed.")))

(try (build!)
     (finally (shutdown-agents)))
