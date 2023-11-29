(ns primitives.storage
  (:require
   primitives.core))

(defonce storage-array (atom '[]))
(defonce parent-less-nodes-atom (atom #{}))
(defonce parent-less-nodes-cache (atom #{}))

(defn node-name
  "returns the name of the node with index `index`"
  [index]
  (nth @storage-array index))

(defn node-name-maps
  "returns a map of indices and node names"
  [storage]
  (map (fn [index] {:index index :id (if (string? (nth storage index))
                                      (node-name (nth storage index))
                                      (nth storage index))}) (range (count storage))))
(defn node-maps
  ;; creates maps with `:id` as the storage entry and `:index` as the index with the collection
  [storage]
  (map (fn [index] {:index index
                    :id (nth storage index)
                   ;; :pos (str index "," (node-height-literal index) "!")
                    })
       (range (count storage))))

(defn node-maps-updated
  ;; creates maps with `:id` as the storage entry and `:index` as the index with the collection
  [storage]
  (map (fn [index] (let [pos (:pos (nth storage index))]
                     (if pos {:index index
                              :id (:id (nth storage index))
                              :pos (str index "," (:pos (nth storage index)) "!")
                              ;; :pos (str index "," (node-height-literal index) "!")
                              }
                         {:index index
                          :id (:id (nth storage index))
                          ;; :pos (str index "," (node-height-literal index) "!")
                          })))(range (count storage))))

(comment
  (map (fn [child-index] (- (+ (mod child-index (int (Math/pow 2 (+ (primitives.storage/p-adic-order 2 child-index) 2))))))) (range 1 1000)))

(map node-name @parent-less-nodes-cache)
(identity @parent-less-nodes-cache)

(defn leaf-location [n]
  (+ (* 2 n) 1))

(defn peak-location [n]
  (+ (* 2 n) 2))

(defn highest-exponent-st-dividing [p n]
  ;; TODO: improve this algorithm - it's inefficient as fuck!
  (last
   (filter #(= 0.0
               (mod (Math/abs n) (Math/pow p %)))
           (range 0 (Math/abs n)))))

(let [log (/ (Math/log 2048) (Math/log 2))] (= 0.0 (- log (int log))))

(defn p-adic-order [p n]
  (if (= 0 n)
    ##Inf
    (highest-exponent-st-dividing p n)))

(comment
  (map #(p-adic-order 2 %) (range 1 100)))

(defn left-child [parent]
  (- parent (* 3 (int (Math/pow 2 (- (p-adic-order 2 parent) 1))))))

(defn right-child [parent]
  (- parent (int (Math/pow 2 (- (p-adic-order 2 parent) 1)))))

(defn children [parent]
  ((juxt left-child right-child) parent))

(defn childedness [node]
  (get {3 :left
        1 :right} (int (mod (/ node (Math/pow 2 (p-adic-order 2 node))) 4))))

(defn parent-index [child-index]
  (+ child-index (mod child-index (int (Math/pow 2 (+ (primitives.storage/p-adic-order 2 child-index) 2))))))


;; O(log2(n)) (barring sort) algo for lowest-common-ancestor of a collection of `leaves`
(defn lowest-common-ancestor-leaves [leaves]
  ;; TODO: remove sort: assume presorted
  ;; find lowest power of two `n` larger than leaf-b
  ;; then recurse `parent-index` on leaf-a `n` times to find said ancestor
  (let [highest-leaf (apply max leaves)
        lowest-leaf (apply min leaves)
        ]
    (nth (iterate parent-index (leaf-location lowest-leaf)) (primitives.core/next-power-of-two (bit-xor (dec lowest-leaf) (dec highest-leaf))))
    )
  )
;; defined using `peak-positions-final`
(defn parent-less-nodes-internal
  "get peaks for leaf-count `n`"
  [n]
  (let [array-size (+ 2 (* 2 n))]
    (reduce #(let [adic (int (Math/pow 2 %2))
                   prepreindex (- array-size (mod array-size adic))
                   ;; TODO: refactor since this only supports two collisions
                   preindex (if (let [log (/ (Math/log prepreindex) (Math/log 2))]
                                  (or
                                   (= 0.0 (- log (int log)))
                                   (some (fn [existing-index] (= existing-index prepreindex)) (reverse %1))))
                              (- prepreindex adic)
                              prepreindex)
                   index (if (let [log (/ (Math/log preindex) (Math/log 2))]
                               (or
                                (= 0.0 (- log (int log)))
                                (some (fn [existing-index] (= existing-index preindex)) (reverse %1))))
                           (- preindex adic)
                           preindex)]
               (conj %1 index))
            []
            (primitives.core/S-n n))))

(defonce leaf-count (atom 0))
(defonce node-count (atom 0))

(defn parent-less-nodes
  "get peaks for current leaf-count or `n`"
  ; TODO: probably using the leaf-count is bad
  ([] (parent-less-nodes @leaf-count))
  ([n]
   (into #{} (parent-less-nodes-internal n))))

;; hacky calculation of parent: if number, take position in (max (position-parent-less-nodes))
(defn position-parentless-nodes [node]
  (first (filter #(= node (nth (into [] (parent-less-nodes)) %)) (range (count (parent-less-nodes))))))

(defn node-height-literal
  "takes the node index `n` and returns the node's height"
  [n]
  (let [child-iterator (iterate left-child n)]
    (count (take-while #(not= (nth child-iterator %)
                              (nth child-iterator (inc %)))
                       (range 3000)))))

(defn parent-less-nodes-sorted-height
  "sorts nodes by height (inverse), with tie-breaker being the node index"
  [nodes]
  (sort #(compare (nth %2 1) (nth %1 1))
        (map (juxt identity node-height-literal) (sort nodes))))

;; this is the L2R bagging from https://hackmd.io/4k2wjlWfTVqgW0Mp4bLSSQ?view
(defn range-node-edges
  "creates a list of the edges between `nodes`, optionally starting names from `starting-index` in lieu of 0"
  ([nodes]
   (let [initial-range-node {:type "range-node" :index 0}]
     (if (> 2 (count nodes))
       (range-node-edges [] [] 0 [])
       (range-node-edges [[(first nodes) initial-range-node] [(second nodes) initial-range-node]] (drop 2 nodes) 0 [initial-range-node]))))

  ([nodes starting-index]
   (let [initial-range-node {:type "range-node" :index starting-index}]
     (if (> 2 (count nodes))
       (range-node-edges [] [] starting-index [])
       (range-node-edges [[(first nodes) initial-range-node] [(second nodes) initial-range-node]] (drop 2 nodes) starting-index [initial-range-node])))
   ;; (let [initial-range-node (str "range-node-" starting-index)]
   ;;   (if (> 2 (count nodes))
   ;;     (range-node-edges [] nodes starting-index [initial-range-node])
   ;;     (range-node-edges [[(first nodes) initial-range-node] [(second nodes) initial-range-node]] (drop 2 nodes) starting-index [initial-range-node])))
   )

  (;; internal function - only accessed via recursion
   [acc remainder depth range-nodes]
   (if (empty? remainder)
     (list acc range-nodes depth)
     ;; (list acc range-nodes)
     (let
      [new-depth (inc depth)
       range-node {:type "range-node" :index new-depth}]
       (range-node-edges (concat acc (map (fn [child] [child range-node])
                                          [(last (last acc)) (first remainder)]))
                         (rest remainder)
                         (inc depth)
                         (conj range-nodes range-node))))))

(defn range-node-edges-reduced [nodes]
  (drop-last (range-node-edges nodes)))

(defn name-index [name]
  (first (filter
          #(= name (nth @storage-array %))
          (range (count @storage-array)))))

(defn path [index]
  (if (contains? (parent-less-nodes) index)
    (concat [(nth @storage-array index)] (last (range-node-edges-reduced (parent-less-nodes))))
    (concat [(nth @storage-array index)] (path (parent-index index)))))

(defn co-path [index]
  (if (contains? (parent-less-nodes) index)
    (map #(nth @storage-array %)
         (filter #(and (not= index %) (< % (count @storage-array))) (parent-less-nodes)))
    (concat
     [(nth @storage-array (first (filter #(not= index %) (children (parent-index index)))))]
     (co-path (parent-index index)))))

(defn non-zero-entries []
  (filter #(not= 0 (:id %)) (node-maps @storage-array)))

(defn parent-ids []
  (filter #(string? (:id %)) (node-maps @storage-array)))
