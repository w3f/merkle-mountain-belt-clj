(ns primitives.storage
  (:require
   state
   primitives.core
   clojure.walk))

(defonce storage-array (atom '[]))
(defonce parent-less-nodes-atom (atom #{}))
(defonce parent-less-nodes-cache (atom #{}))

(def storage-maps {:internal state/node-map
                   :peak state/node-map
                   :range state/range-nodes
                   :belt state/belt-nodes})

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
  (last
   (filter #(= 0.0
               (mod (Math/abs n) (Math/pow p %)))
           (range 0 (Math/abs n)))))

(let [log (/ (Math/log 2048) (Math/log 2))] (= 0.0 (- log (int log))))

(defn p-adic-order [p n]
  (if (= 0 n)
    ##Inf
    (highest-exponent-st-dividing p n)))

(defn two-adic-order [n]
  (if (= 0 n)
    ##Inf
    (Integer/numberOfTrailingZeros n)))

(comment
  (two-adic-order 0)
  (map #(p-adic-order 2 %) (range 1 100)))

(defn left-child [parent]
  (- parent (* 3 (int (Math/pow 2 (- (two-adic-order parent) 1))))))

(defn right-child [parent]
  (- parent (int (Math/pow 2 (- (two-adic-order parent) 1)))))

(defn children [parent]
  ((juxt left-child right-child) parent))

(comment (children 6))

;; (defn descendants [parent]
;;   (let [children (children parent)]
;;     {:index parent
;;      :children (if (every? #(and (pos? %) (not= parent %)) children) (map descendants children) '())}))

(defn descendants [parent]
  (if (even? parent)
    (let [children (children parent)]
      (if (every? #(and (pos? %) (not= parent %)) children) (into [] (map descendants children)) parent))
    parent))

(defn descendants-with-terminals [parent terminals]
  (if (and (even? parent) (not (contains? terminals parent)))
    (let [parent ((juxt identity state/name-lookup) parent)
          children ((juxt identity state/name-lookup) (children parent))]
      (if (every? #(and (pos? %) (not= (first parent) (first %))) children) (into [] (map #(descendants-with-terminals (first %) terminals) children)) parent))
    parent))

(comment (defn indices-to-names [structure]
           (cojure.walk/postwalk
            #(if (int? %) (nth @state/node-array %) %)
            structure))

         (clojure.walk/postwalk
          ;; merge two sets #{1 2 3} #{2 3 4} => #{1 2 3 4}
          (fn ([a b] (into #{} (concat a b)))
            ([a] a))
          (clojure.walk/postwalk
           #(if (int? %) (nth @state/node-array %) %)
           (descendants-with-terminals 20 #{18})))

         (clojure.walk/postwalk
          ;; merge two sets #{1 2 3} #{2 3 4} => #{1 2 3 4}
          (fn [i] (if (vector? i) (apply concat (map #(nth @state/node-array %) i)) i))
          (descendants-with-terminals 20 #{18}))

         (nth @state/node-array 20)
         (descendants 20)
         (descendants-with-terminals 20 #{18})
         (state/indices-to-names [20 (descendants-with-terminals 20 #{})]))

(comment (flatten (descendants 124))
         (left-child 115))

(defn childedness [node]
  (get {3 :left
        1 :right} (int (mod (/ node (Math/pow 2 (two-adic-order node))) 4))))

(defn parent-index [child-index]
  (+ child-index
     (mod child-index
          (int (Math/pow 2
                         (+ (primitives.storage/two-adic-order child-index) 2))))))

(comment
  (Math/pow 2 (- (two-adic-order 4) 1))
  (children (parent-index 1))
  (descendants 4)
  (map #(parent-index (leaf-location %)) (range 1 (inc 256))))

;; O(log2(n)) (barring sort) algo for lowest-common-ancestor of a collection of `leaves`
(defn lowest-common-ancestor-leaves [leaves]
  ;; TODO: remove sort: assume presorted
  ;; find lowest power of two `n` larger than leaf-b
  ;; then recurse `parent-index` on leaf-a `n` times to find said ancestor
  (let [highest-leaf (apply max leaves)
        lowest-leaf (apply min leaves)]
    (nth (iterate parent-index (leaf-location lowest-leaf)) (primitives.core/next-power-of-two (bit-xor (dec lowest-leaf) (dec highest-leaf))))))

(comment
  (Integer/toBinaryString (bit-xor 2r111 2r10))
  (dec (primitives.core/next-power-of-two (bit-xor 2r111 2r10)))
  (reduce (fn [acc n] (conj acc (+ (last acc) (int (Math/pow 2 n))))) [0] (primitives.core/S-n 30))
  (primitives.core/next-power-of-two 30))

;; (defn segregate-by-peak [leaves leaf-count]
;;   (let [peak-positions (primitives.core/peak-positions-final leaf-count)]
;;     (reduce (fn [acc leaf]
;;               (let [peak (first (filter #(= leaf (nth @storage-array %)) peak-positions))]
;;                 (assoc acc peak (conj (get acc peak []) leaf))))
;;             {}
;;             leaves)))

;; (defn lowest-common-ancestor [leaf-a-index])

;; (children (parent-index 3))

;; (defn name-lookup)

;; (defn padded-index-only-debugging
;;   "convert the unpadded index (i.e. the most compact form) to the actual one. For instance, the unpadded node array would be `[#{1} #{2} #{1 2}]`, but it's actually stored as `[0 0 0 #{1} 0 #{2} #{1 2}]`"
;;   [unpadded-index]
;;   (nth (filter #(not= 0 %) @state/node-array) unpadded-index)
;;   )

;; (padded-index-only-debugging 1)

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

(comment
  (let [n 10]
    [(primitives.core/S-n 10)
     (parent-less-nodes 10)]))

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

(comment
  ;; get depth of last leaf
  (apply min (map #(node-height-literal (nth (parent-less-nodes-internal %) (- (count (parent-less-nodes-internal %)) 9))) (range 1000 1500)))
  (map #(node-height-literal (last (parent-less-nodes-internal %))) (range 1 10)))

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
