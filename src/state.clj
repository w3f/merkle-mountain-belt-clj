(ns state)

;; state containers
(def lastP (atom #{}))

;; (def R-count (atom 0))

(def mergeable-stack (atom []))

(def leaf-count (atom 0))

(def node-map (atom {}))
(def node-array (atom []))
(def belt-nodes (atom {}))
;; NOTE: this is a hack to actually point to the dummy range node
;; TODO: potentially find more elegant solution since this hack may introduce bugs
(def root-belt-node (atom #{}))
(def range-nodes (atom {}))
;; (def belt-children (atom {}))

(def pointers (atom #{}))

;; TODO: maybe create new ns for the storage atoms
(defn reset-atoms-from-cached! [cached]
  (reset! node-map (:node-map cached))
  (reset! node-array (:node-array cached))
  (reset! mergeable-stack (:mergeable-stack cached))
  (reset! leaf-count (:leaf-count cached))
  (reset! lastP (:lastP cached))
  (reset! belt-nodes (:belt-nodes cached))
  (reset! root-belt-node (:root-belt-node cached))
  (reset! range-nodes (:range-nodes cached)))

(defn current-atom-states []
  {:node-map @node-map
   :node-array @node-array
   :mergeable-stack @mergeable-stack
   :leaf-count @leaf-count
   :lastP @lastP
   :belt-nodes @belt-nodes
   :root-belt-node @root-belt-node
   :range-nodes @range-nodes})
