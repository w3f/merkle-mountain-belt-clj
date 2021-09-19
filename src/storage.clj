(ns storage)

(def storage-array (atom '[]))
(def leaf-count (atom 0))
(def node-count (atom 0))

(defn leaf-location [n]
  (+ (* 2 n) 1))

(defn peak-location [n]
  (+ (* 2 n) 2))

(defn highest-exponent-st-dividing [p n]
  (last
   (filter #(= 0.0
               (mod (Math/abs n) (Math/pow p %)))
           (range 0 (Math/abs n)))))

(defn p-adic-order [p n]
  (if (= 0 n)
    ##Inf
    (highest-exponent-st-dividing p n)
    ))

(defn add-internal [item index]
  (let [array-len (count @storage-array)
        ;; incidentally correct since index is calculated starting at 1 in lieu of 0
        zero-leaves (- index array-len)]
    (swap! storage-array concat (repeat zero-leaves 0) (list item))
    ))

(defn add-leaf [leaf]
  (do
    ;; increase the leaf index
    (swap! leaf-count inc)
    (add-internal leaf (leaf-location @leaf-count))
    (if
        (not= (+ @leaf-count 1) (int (Math/pow 2 (p-adic-order 2 (+ @leaf-count 1)))))
      (add-internal (str "p-" (swap! node-count inc)) (peak-location @leaf-count))
      )
    )
  )

(defn left-child [parent]
  (- parent (* 3 (int (Math/pow 2 (- (p-adic-order 2 parent) 1))))))

(defn right-child [parent]
  (- parent (int (Math/pow 2 (- (p-adic-order 2 parent) 1)))))

(defn children [parent]
  ((juxt left-child right-child) parent))

(defn node-maps [storage]
  (map (fn [index ] {:index index :id (nth storage index)}) (range (count storage))))

(defn non-zero-entries []
  (filter #(not= 0 (:id %)) (node-maps @storage-array)))

(defn parents []
  (filter #(string? (:id %)) (node-maps @storage-array)))

(defn parent-index [child-index]
  (+ child-index (mod child-index (int (Math/pow 2 (+ (p-adic-order 2 child-index) 2))))))

(defn node-name [index]
  (nth @storage-array index))

(defn name-index [name]
  (first (filter #(= name (nth @storage-array %))(range (count @storage-array)))))

;; strategy: get parent indices and filter for nodes where index exceeds length of storage-array
(defn parent-less-nodes []
  (->>
   (filter
    #(and
      (< (count @storage-array) (second %))
      (not= 0 (node-name (first %)))
      )
    (map (juxt identity parent-index) (range 1 (count @storage-array))))
   flatten
   (filter #(< % (count @storage-array)))
   (into #{})
   )
  )

(do
  (reset! storage-array '[])
  (reset! leaf-count 0)
  (reset! node-count 0)
  (println "------")
  (doall (map #(add-leaf %) (range 1 15)))
  ;; (println (range (count @storage-array)))
  ;; (println @storage-array)
  (apply str (map #(str %1 ": " %2 " |") (range (count @storage-array)) @storage-array))
  )

;; trees of children
(map (juxt left-child right-child) [6 10 12 14])
(map (juxt identity children) (filter #(re-matches #"p-.*" (str (nth @storage-array %))) (range (count @storage-array))))
(filter #(not= 0 (nth @storage-array %)) (range (count @storage-array)))
(map (juxt identity children) (filter #(re-matches #"p-.*" (str (nth @storage-array %))) (range (count @storage-array))))

;; confirm that all non-zero entries are covered by the tree
(=
 #{}
 (clojure.set/difference
  (into #{} (filter #(not= 0 (nth @storage-array %)) (range (count @storage-array))))
  (into #{} (flatten (map (juxt identity left-child right-child) (filter #(re-matches #"p-.*" (str (nth @storage-array %))) (range (count @storage-array))))))
  ))

;; [0 0 0 1 0 2 x 3 0 4 x 5 x 6 x 7 0 8 x 9 x]

(map #(p-adic-order % 3) (range 1 500))

(defn path [index]
  (if (contains? (parent-less-nodes) index)
      [(nth @storage-array index)]
      (concat [(nth @storage-array index)] (path (parent-index index)))
     ))

(defn co-path [index]
  (if (contains? (parent-less-nodes) index)
      (map #(nth @storage-array %)
           (filter #(and (not= index %) (< % (count @storage-array))) (parent-less-nodes)))
     (concat
      [(nth @storage-array (first (filter #(not= index %) (children (parent-index index)))))]
      (co-path (parent-index index)))
     ))

;; test identification of children
(= 10 (parent-index 7))
(= 10 (parent-index 9))

;; test identification of children
(= 12 (parent-index 6))
(= 12 (parent-index 10))

;; l2r bagging

(defn range-node-edges
  ([nodes]
   (let [range-node "range-node-0"]
     (range-node-edges [[(first nodes) "range-node-0"] [(second nodes) "range-node-0"]] (drop 2 nodes) 0 [range-node])))

  ([acc remainder depth range-nodes]
   (if (empty? remainder)
     (list acc range-nodes)
     (let
         [new-depth (inc depth)
          range-node (str "range-node-" new-depth)]
         (range-node-edges (concat acc (map (fn [child] [child range-node])
                                         [(last (last acc)) (first remainder)]))
                        (rest remainder)
                        (inc depth)
                        (conj range-nodes range-node)))
     ))
  )

