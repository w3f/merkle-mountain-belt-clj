(ns state
  (:require
   clojure.walk))

;; state containers. phantom hash is [] (compact [lo hi] representation).
(def rightmostP (atom []))

;; (def R-count (atom 0))

(def mergeable-stack (atom []))

(def leaf-count (atom 0))

(def node-map (atom {}))
(def node-array (atom []))
(def belt-nodes (atom {}))
;; NOTE: this is a hack to actually point to the dummy range node
;; TODO: potentially find more elegant solution since this hack may introduce bugs
(def root-belt-node (atom []))
(def range-nodes (atom {}))
;; (def belt-children (atom {}))

(def pointers (atom #{}))

(def hash-count (atom 0))

;; TODO: maybe create new ns for the storage atoms
(defn reset-atoms-from-cached! [cached]
  (reset! node-map (:node-map cached))
  (reset! node-array (:node-array cached))
  (reset! mergeable-stack (:mergeable-stack cached))
  (reset! leaf-count (:leaf-count cached))
  (reset! rightmostP (:rightmostP cached))
  (reset! belt-nodes (:belt-nodes cached))
  (reset! root-belt-node (:root-belt-node cached))
  (reset! range-nodes (:range-nodes cached))
  (reset! hash-count (or (:hash-count cached) 0)))

(defn current-atom-states []
  {:node-map @node-map
   :node-array @node-array
   :mergeable-stack @mergeable-stack
   :leaf-count @leaf-count
   :rightmostP @rightmostP
   :belt-nodes @belt-nodes
   :root-belt-node @root-belt-node
   :range-nodes @range-nodes})

;; TODO: maybe add the index to the nodes when creating them
(defn name-lookup [name]
  (first (filter #(= name (nth @node-array %)) (range (count @node-array)))))

(defn indices-to-names [structure]
  (clojure.walk/postwalk
   #(if (int? %) (nth @node-array %) %) structure))

;; --- compact [lo hi] <-> #{} hash translation (for comparing against #{}-form cached refs) ---

(defn set-hash->compact
  "Translate a #{}-form hash — a contiguous set of leaf indices, or #{} (phantom) —
   to the compact [lo hi] form (or []). Asserts contiguity, since only contiguous
   ranges are valid hashes."
  [s]
  (if (empty? s)
    []
    (let [lo (apply min s) hi (apply max s)]
      (assert (= (count s) (inc (- hi lo))) (str "non-contiguous hash set: " s))
      [lo hi])))

(defn remap-to-compact
  "Deep-translate a #{}-form structure (e.g. a cached state map) to compact [lo hi]
   form, replacing every set with its [lo hi] span. Every set in these structures is
   a hash, so this is unambiguous."
  [form]
  (clojure.walk/postwalk
   #(if (set? %) (set-hash->compact %) %)
   form))
