(ns primitives.proof
  (:require [state :refer [node-array node-map]]
            [primitives.storage :refer [storage-maps]]
            [clojure.set]))

(def type-rank
  "used for classifying possible parent/child types"
  (zipmap
   [:internal :peak :range :belt]
   (range)))

(defn parent-type-contenders
  [type]
  (let [rank (get type-rank type)]
    (if (= rank (:peak type-rank))
      (list (get (clojure.set/map-invert type-rank) (inc rank)))
      (if (= rank (:belt type-rank))
        (list :belt)
        (list type (get (clojure.set/map-invert type-rank) (inc rank)))))))


;; DONE: refactor this since logic is somewhat clunky
(defn get-parent
  ([child]
   (let [parent-type-contenders (parent-type-contenders (:type child))]
     (first (filter #(and % (not= child %)) (map #(get @% (:parent child)) (vals (select-keys storage-maps parent-type-contenders)))))))
  ([child expected-parent]
   (let [parent (get-parent child)]
     (if (= expected-parent (:type parent))
       parent
       (throw (Exception. (str "parent type expected: " expected-parent "\nactual parent type: " (:type parent) " " (:hash child) " " (:type child))))))))

;; DONE: can simplify if use phantom belt node since
;; then have left child always have same type as parent
(defn child-type
  ([type]
   (child-type type nil))
  ([type child-leg]
   (let [rank (get type-rank type)]
     (if (<= rank (:peak type-rank))
       :internal
       (if (= child-leg :left)
         type
         (if (= child-leg :right)
           (get (clojure.set/map-invert type-rank) (dec rank))
           (throw (Exception. "no child-leg specified"))))))))

(defn get-child [parent child-leg]
  (let [child-type (child-type (:type parent) child-leg)]
    (second (find @(get storage-maps child-type) (get parent child-leg)))))

(defn get-sibling [entry]
  (let [parent (get-parent entry)]
    (if (= (:hash entry) (:left parent))
      (get-child parent :right)
      (get-child parent :left))))

(defn sibling-index [n-index]
  (first (filter #(not= n-index %) (primitives.storage/children (primitives.storage/parent-index n-index)))))

;; siblings of ephemeral nodes are also ephemeral
(defn co-path-ephemeral
  ([entry accumulator]
   (if (:parent entry)
     (co-path-ephemeral (get-parent entry) (if (= (:hash entry) (:parent entry))
                                             accumulator
                                             (concat accumulator [(:hash (get-sibling entry))])))
     accumulator)
   ;; (concat [(:hash (get-sibling entry))] (if (:parent entry) (co-path-ephemeral (get-parent entry))))
   ))


(defn co-path-internal-indices
  "returns the indices of co-path items"
  [index accumulator max-index ephemeral?]
  ;; if the node's parent's index is less than the number of nodes in the node
  ;; array, then the parent node is in the node array, hence we can use the
  ;; node array to get the sibling
  (if (<= (primitives.storage/parent-index index) max-index)
    (co-path-internal-indices (primitives.storage/parent-index index) (concat accumulator [(sibling-index index)]) max-index ephemeral?)
    ;; #dbg
    ;; #dbg
    (if ephemeral? (co-path-ephemeral (get @node-map (nth @node-array index))
                                      (let [sibling-index (sibling-index index)]
                                        (if (< sibling-index (count @node-array))
                                          ;; #dbg
                                          (concat accumulator [sibling-index])
                                          accumulator)
                                        accumulator
                                        ;; (concat accumulator [(nth @node-array sibling-index)])
                                        ))
        accumulator)))


(defn co-path-internal
  "remaps co-path indices to \"hashes\""
  [index accumulator max-index ephemeral?]
   ;; if the node's parent's index is less than the number of nodes in the node
   ;; array, then the parent node is in the node array, hence we can use the
   ;; node array to get the sibling
  (->> (co-path-internal-indices index accumulator (or max-index (count @node-array)) ephemeral?)
       (map #(if (int? %)
               (nth @node-array %)
               %))))

(defn co-path-internal-v0
  "remaps co-path indices to \"hashes\""
  [index accumulator max-index ephemeral?]
  ;; if the node's parent's index is less than the number of nodes in the node
  ;; array, then the parent node is in the node array, hence we can use the
  ;; node array to get the sibling
  (if (<= (primitives.storage/parent-index index) max-index)
    (co-path-internal-v0 (primitives.storage/parent-index index) (concat accumulator [(nth @node-array (sibling-index index))]) max-index ephemeral?)
    (if ephemeral? (co-path-ephemeral (get @node-map (nth @node-array index))
                                      (let [sibling-index (sibling-index index)]
                                        (if (< sibling-index (count @node-array))
                                          ;; #dbg
                                          (concat accumulator [(nth @node-array sibling-index)])
                                          accumulator)
                                        accumulator
                                        ;; (concat accumulator [(nth @node-array sibling-index)])
                                        ))
        accumulator)))
