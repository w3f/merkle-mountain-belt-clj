(ns storage
  (:require [core]))

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
;; TODO fix this inefficient fucker
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
   (into [])
   ;; (into #{})
   )
  )

(do
  (reset! storage-array '[])
  (reset! leaf-count 0)
  (reset! node-count 0)
  (println "------")
  (doall (map #(add-leaf %) (range 1 1222)))
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
;; TODO: doesn't work for n=1222 -> investigate!
(=
 #{}
 (clojure.set/difference
  (into #{} (filter #(not= 0 (nth @storage-array %)) (range (count @storage-array))))
  (into #{} (flatten (map (juxt identity left-child right-child) (filter #(re-matches #"p-.*" (str (nth @storage-array %))) (range (count @storage-array))))))
  ))

;; [0 0 0 1 0 2 x 3 0 4 x 5 x 6 x 7 0 8 x 9 x]

(map #(p-adic-order % 3) (range 1 500))

;; this is the L2R bagging from https://hackmd.io/4k2wjlWfTVqgW0Mp4bLSSQ?view
(defn range-node-edges
  "creates a list of the edges between `nodes`, optionally starting names from `starting-index` in lieu of 0"
  (
   [nodes]
   (let [initial-range-node "range-node-0"]
     (if (> 2 (count nodes))
       (range-node-edges [] [] 0 [])
       (range-node-edges [[(first nodes) initial-range-node] [(second nodes) initial-range-node]] (drop 2 nodes) 0 [initial-range-node]))))

  (
   [nodes starting-index]
   (let [initial-range-node (str "range-node-" starting-index)]
     (if (> 2 (count nodes))
       (range-node-edges [] [] starting-index [])
       (range-node-edges [[(first nodes) initial-range-node] [(second nodes) initial-range-node]] (drop 2 nodes) starting-index [initial-range-node])))
   ;; (let [initial-range-node (str "range-node-" starting-index)]
   ;;   (if (> 2 (count nodes))
   ;;     (range-node-edges [] nodes starting-index [initial-range-node])
   ;;     (range-node-edges [[(first nodes) initial-range-node] [(second nodes) initial-range-node]] (drop 2 nodes) starting-index [initial-range-node])))
   )

  (
   ;; internal function - only accessed via recursion
   [acc remainder depth range-nodes]
   (if (empty? remainder)
     (list acc range-nodes depth)
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

(defn range-node-edges-reduced [nodes]
  (drop-last (range-node-edges nodes)))

(defn path [index]
  (if (contains? (parent-less-nodes) index)
    (concat [(nth @storage-array index)] (last (range-node-edges-reduced (parent-less-nodes))))
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

(defn bag-left-to-right
  ([remainder]
   (bag-left-to-right [(first remainder) (second remainder)] (drop 2 remainder)))

  ([current-bagging remainder]
   (if (empty? remainder)
     current-bagging
     (bag-left-to-right [current-bagging (first remainder)] (rest remainder)))))

(defn range-node-map
  ([nodes]
   (range-node-map {:id "range-node-1" :children {:left {:id (first nodes)} :right {:id (second nodes)}}} (drop 2 nodes) 1))

  ([acc remainder depth]
   (if (empty? remainder)
     acc
     (range-node-map {:id (str "range-node-" (inc depth))
                        :left acc
                        :right (first remainder)}
                       (rest remainder)
                       (inc depth))
     ))
  )

;; hacky calculation of parent: if number, take position in (max (position-parent-less-nodes))
(defn position-parentless-nodes [node]
  (first (filter #(= node (nth (into [] (parent-less-nodes)) %)) (range (count (parent-less-nodes))))))

(defn left-to-right-parent [node]
  (if (number? node)
    (str "range-node-" (max 0 (- (position-parentless-nodes node) 1)))
    (let []
      (str "range-node-" (inc (Integer. (first (re-seq #"[0-9]+" node))))))))

(defn left-to-right-range-node-path [node]
  (left-to-right-parent node)
  )

;; does n=1222 correspond to merkle tree list '(9  8  8  7  5  4  3  3  2  1)?
(= 1222 (->
         (reduce + 0 (map #(Math/pow 2 %) '(9  8  8  7  5  4  3  3  2  1)))
         int))
;; -> true

(def parent-less-nodes-cache (parent-less-nodes))

(defn binary-repr-of-n [n]
  (Integer/toBinaryString n))

(defn S-n [n]
  (let [bits (map (comp #(Integer. %) str) (binary-repr-of-n (inc n)))
        reversed-bits (reverse bits)]
    (reverse (map
              #(+ % (nth reversed-bits %))
              (range (dec (count bits)))))))

(defn s-m-of-n [m n]
  (nth (reverse (S-n n)) m))

(S-n 4)
(S-n 1222)
(S-n 1232)
(S-n 3)
(s-m-of-n 7 1222)


(defn storage-add! [node]
  (swap! core/storage assoc-in [(core/hash-node node)] node))

(defn belt-depth [node]
  (if (core/has-children? node)
    (+ 1
       (apply max (map belt-depth (core/children node))))
    1)
  )

