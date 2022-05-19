(ns storage
  (:require [core]
            [primitives.core]
            [primitives.storage :refer [left-child right-child]]))

(defonce storage-array (atom '[]))
(defonce parent-less-nodes-atom (atom #{}))
(defonce parent-less-nodes-cache (atom #{}))
(defonce peaks-accumulator (atom []))

(defn leaf-location [n]
  (+ (* 2 n) 1))

(defn peak-location [n]
  (+ (* 2 n) 2))

(comment
  (aget (bytes (byte-array (byte 4))) 1)
  (bit-and 1 1))



(defn children [parent]
  ((juxt left-child right-child) parent))

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
    (swap! parent-less-nodes-atom #(conj % (leaf-location @leaf-count)))
    (if
        (not= (+ @leaf-count 1) (int (Math/pow 2 (primitives.storage/p-adic-order 2 (+ @leaf-count 1)))))
      (do
        (add-internal (str "p-" (swap! node-count inc)) (peak-location @leaf-count))
        (swap! parent-less-nodes-atom #(conj % (peak-location @leaf-count)))
        (swap! parent-less-nodes-atom #(apply disj % (children (peak-location @leaf-count)))))
      )
    (swap! peaks-accumulator #(conj % @parent-less-nodes-atom)))
  )

(comment
  (= (primitives.core/S-n @leaf-count)
    (reverse (sort (map node-height-literal @parent-less-nodes-cache)))))

(map (juxt identity node-height-literal) @parent-less-nodes-cache)

(defn node-maps
  ;; creates maps with `:id` as the storage entry and `:index` as the index with the collection
  [storage]
  (map (fn [index] {:index index
                   :id (nth storage index)
                   ;; :pos (str index "," (node-height-literal index) "!")
                   }) (range (count storage))))

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
                         }
                        )
                    )) (range (count storage))))

(comment
  (storage/node-maps (into [] (flatten (core/belted-edges)))))
(comment
  (storage/node-maps (into [] (flatten (core/belted-nodes)))))

(defn node-name [index]
  (nth @storage-array index))

(defn node-name-maps [storage]
  (map (fn [index] {:index index :id (if (string? (nth storage index))
                                      (node-name (nth storage index))
                                      (nth storage index))}) (range (count storage))))

(defn non-zero-entries []
  (filter #(not= 0 (:id %)) (node-maps @storage-array)))

(defn parents []
  (filter #(string? (:id %)) (node-maps @storage-array)))

(comment
  (map (fn [child-index] (- (+ (mod child-index (int (Math/pow 2 (+ (primitives.storage/p-adic-order 2 child-index) 2))))))) (range 1 1000)))

(map node-name @parent-less-nodes-cache)
(identity @parent-less-nodes-cache)

(defn name-index [name]
  (first (filter #(= name (nth @storage-array %))(range (count @storage-array)))))

;; strategy: get parent indices and filter for nodes where index exceeds length of storage-array
;; TODO fix this inefficient fucker
(defn parent-less-nodes-v1 []
  (->>
   (filter
    #(and
      (< (count @storage-array) (second %))
      (not= 0 (node-name (first %)))
      )
    (map (juxt identity parent-index) (range 1 (count @storage-array))))
   flatten
   (filter #(< % (count @storage-array)))
   ;; (into [])
   (into #{})
   )
  )

(clojure.set/difference @parent-less-nodes-cache @parent-less-nodes-atom)

(defn run [n]
  (do
   (reset! storage-array '[])
   (reset! leaf-count 0)
   (reset! node-count 0)
   (reset! peaks-accumulator [])
   (reset! parent-less-nodes-atom #{})
   (println "------")
   (doall (map #(add-leaf %) (range 1 n)))
   ;; (doall (map #(add-leaf %) (range 1 1223)))
   ;; (doall (map #(add-leaf %) (range 1 1224)))
   (reset! parent-less-nodes-cache (parent-less-nodes))
   ;; (println (range (count @storage-array)))
   ;; (println @storage-array)
   (let [print-len 50] (apply str (map #(str %1 ": " %2 " |") (range print-len) (take print-len @storage-array))))
   ))

(comment
  (run 1223))

(comment
  (nth @peaks-accumulator (dec 4))
  (first @peaks-accumulator))
(map-indexed #(identity [(inc %1) (- (apply max %2) (apply min %2))]) (take 100 @peaks-accumulator))
(comment (apply min (map (comp last butlast primitives.core/S-n) (range 3 1E4))))

(defonce storage-array-5000 (atom @storage-array))
(defonce parent-less-nodes-atom-5000 (atom @parent-less-nodes-atom))
(count @storage-array-5000)

(comment
  (reset! parent-less-nodes-cache (parent-less-nodes))
  (empty? (clojure.set/difference @parent-less-nodes-cache @parent-less-nodes-atom))
  (empty? (clojure.set/difference (parent-less-nodes) @parent-less-nodes-atom)))

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

(map #(primitives.storage/p-adic-order % 3) (range 1 500))

;; this is the L2R bagging from https://hackmd.io/4k2wjlWfTVqgW0Mp4bLSSQ?view
(defn range-node-edges
  "creates a list of the edges between `nodes`, optionally starting names from `starting-index` in lieu of 0"
  (
   [nodes]
   (let [initial-range-node {:type "range-node" :index 0}]
     (if (> 2 (count nodes))
       (range-node-edges [] [] 0 [])
       (range-node-edges [[(first nodes) initial-range-node] [(second nodes) initial-range-node]] (drop 2 nodes) 0 [initial-range-node]))))

  (
   [nodes starting-index]
   (let [initial-range-node {:type "range-node" :index starting-index}]
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
     ;; (list acc range-nodes)
     (let
         [new-depth (inc depth)
          range-node {:type "range-node" :index new-depth}]
       (range-node-edges (concat acc (map (fn [child] [child range-node])
                                          [(last (last acc)) (first remainder)]))
                         (rest remainder)
                         (inc depth)
                         (conj range-nodes range-node)))
     ))
  )

(defn range-node-edges-reduced [nodes]
  (drop-last (range-node-edges nodes)))

(apply max (map #(double (- (parent-index %) %)) (range 1 1000 2)))
(apply max (map #(double (/ (parent-index %) %)) (range 1 1000 2)))
(map #(primitives.storage/p-adic-order 2 %) (range 1 1000 2))
;; (map #(/ % parent-index) (range 1 500 2))

(defn path [index]
  (if (contains? (parent-less-nodes) index)
    (concat [(nth @storage-array index)] (last (range-node-edges-reduced (parent-less-nodes))))
    (concat [(nth @storage-array index)] (path (parent-index index)))
    ))

(drop-last (range-node-edges (parent-less-nodes)))

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

;; (bag-left-to-right (map #(core/leaf %) (primitives.core/S-n 7)))

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

(defn left-to-right-parent [node]
  (if (number? node)
    (str "range-node-" (max 0 (- (position-parentless-nodes node) 1)))
    (let []
      (str "range-node-" (inc (Integer. (first (re-seq #"[0-9]+" node))))))))

(defn left-to-right-range-node-path [node]
  (left-to-right-parent node))

(into #{} (flatten (drop-last (range-node-edges (parent-less-nodes)))))
(drop-last (range-node-edges (parent-less-nodes)))

;; (defn range-nodes)

(nth (range-node-edges [1 2 3 4 5]) 1)

(nth (range-node-edges @parent-less-nodes-cache) 1)
;; (primitives.core/S-n 19)

(defn range-node-map-unfold [range-node-map])

;; (co-path (name-index "p-5"))
(non-zero-entries)

;; does n=1222 correspond to merkle tree list '(9  8  8  7  5  4  3  3  2  1)?
(= 1222 (->
         (reduce + 0 (map #(Math/pow 2 %) '(9  8  8  7  5  4  3  3  2  1)))
         int))
;; -> true

(comment (map (juxt identity node-height-literal) @parent-less-nodes-cache))

(comment (map first (parent-less-nodes-sorted-height @parent-less-nodes-cache)))
(comment ([1536 9] [1792 8] [2304 8] [2432 7] [2400 5] [2416 4] [2424 3] [2440 3] [2444 2] [2446 1]))

;; DONE: intermediate algo (still requires reordering of image wrt. peak order in storage array)
(defn peak-positions-intermediate
  "intermediate algo for getting peaks"
  [n]
  (map
   #(str (= %1 %2) ":" %1 ":" %2)
   (first (reduce
           #(let [p 2
                  ;; n 1222
                  array-size (dec (second %1))
                  height %2
                  rank 0
                  adic (int (Math/pow p height))
                  preindex (- array-size (mod array-size adic))
                  index (if (let [log (/ (Math/log preindex) (Math/log 2))] (= 0.0 (- log (int log))))
                          (- preindex adic)
                          preindex)
                  ]
              (identity [(conj (first %1) index) index])
              ;; ()
              ;; [index (/ index adic)]
              )
           [[] (+ 3 (* 2 n))]
           ;; (reverse (primitives.core/S-n 1222))
           (map #(highest-exponent-st-dividing 2 %) (reverse (sort (nth @peaks-accumulator (dec n)))))
           ))
   (into [] (reverse (sort (into [] (nth @peaks-accumulator (dec n)))))) )
  )

;; DONE: final algo
(defn peak-positions-final
  "verifies that, up to `n`, the peak positions are correctly calculated"
  [m]
  (every? true?
         (pmap
          (fn [n]
            (=
             (let [array-size (+ 2 (* 2 n))]
               (sort (reduce #(let [adic (int (Math/pow 2 %2))
                                    prepreindex (- array-size (mod array-size adic))
                                    preindex (if (let [log (/ (Math/log prepreindex) (Math/log 2))]
                                                   (or
                                                    (= 0.0 (- log (int log)))
                                                    (some (fn [existing-index] (= existing-index prepreindex)) (reverse %1))
                                                    ))
                                               (- prepreindex adic)
                                               prepreindex)
                                    index (if (let [log (/ (Math/log preindex) (Math/log 2))]
                                                (or
                                                 (= 0.0 (- log (int log)))
                                                 (some (fn [existing-index] (= existing-index preindex)) (reverse %1))
                                                 ))
                                            (- preindex adic)
                                            preindex)
                                    ]
                                (conj %1 index)
                                )
                             []
                             (primitives.core/S-n n)))
               )
             (sort (into [] (nth @peaks-accumulator (dec n))))
             ))
          (range 1222 (inc (count @peaks-accumulator)))
          )))

(map-indexed (fn [index n] [(str "n: " index ", exponent: " (highest-exponent-st-dividing 2 n) )]) (range 40))
(map-indexed (fn [index n] [index (highest-exponent-st-dividing 2 n)]) (range 40))

(count @peaks-accumulator)

(comment
  (sort (into [] (nth @peaks-accumulator (dec 1222)))))

(comment
  (map (juxt identity #(highest-exponent-st-dividing 2 %)) (reverse (sort (nth @peaks-accumulator (dec 1221))))))

(comment
  (nth (reverse (sort (nth @peaks-accumulator (dec 1221)))) 0)
  (nth (reverse (sort (nth @peaks-accumulator (dec 1225)))) 0)
  (nth (reverse (sort (nth @peaks-accumulator (dec 1222)))) 3)
  (highest-exponent-st-dividing 2 (nth (reverse (sort (nth @peaks-accumulator (dec 1222)))) 3)))

;; DONE: concrete instance of intermediate algo
(comment
  (every? true?
         (pmap (fn [n] (= (first (reduce
                                 #(let [p 2
                                        ;; n 1222
                                        array-size (dec (second %1))
                                        height %2
                                        rank 0
                                        adic (int (Math/pow p height))
                                        preindex (- array-size (mod array-size adic))
                                        index (if (let [log (/ (Math/log preindex) (Math/log 2))] (= 0.0 (- log (int log))))
                                                (- preindex adic)
                                                preindex)
                                        ]
                                    (identity [(conj (first %1) index) index])
                                    ;; ()
                                    ;; [index (/ index adic)]
                                    )
                                 [[] (+ 3 (* 2 n))]
                                 (map #(highest-exponent-st-dividing 2 %) (reverse (sort (nth @peaks-accumulator (dec n)))))                   ))
                         (reverse (sort (into [] (nth @peaks-accumulator (dec n)))))))
               (range 1 2000)
               )
         ))

(primitives.core/S-n 1222)
(comment
  (reverse (sort (map #(highest-exponent-st-dividing 2 %) (nth @peaks-accumulator (dec 1222))))))
(comment
  (parent-index 1222))
(comment
  (clojure.set/intersection (nth @peaks-accumulator (dec 1222)) (into #{} (map #(* (int (Math/pow 2 9)) %) (range 9)))))

(count @peaks-accumulator)

(highest-exponent-st-dividing 2 10000)

(primitives.core/S-n 1222)

(defn s-m-of-n [m n]
  (nth (reverse (primitives.core/S-n n)) m))

(primitives.core/S-n 4)
(primitives.core/S-n 1222)
(primitives.core/S-n 1232)
(primitives.core/S-n 3)
(s-m-of-n 7 1222)

;; (let [reversed-bits (map (comp #(Integer/parseUnsignedInt %) str) (reverse (binary-repr-of-n 1222)))]
;;   (map #(+ %1 %2) reversed-bits (range (count reversed-bits))))

(map (reverse (map (comp #(Integer. %) str) (into [] "123"))))

;; verify that the number of peaks == (dec binary length of leaf-count)
(= (dec (count (binary-repr-of-n @leaf-count)))
   (count @parent-less-nodes-cache))

;; see https://hackmd.io/4k2wjlWfTVqgW0Mp4bLSSQ?view#An-initial-description-without-bagging
;; to get mountain heights
;; (defn s-i )

(identity @parent-less-nodes-cache)

(defn find-right-most-parentless [])

;; (identity @core/storage)

(defn storage-add! [node]
  (swap! core/storage assoc-in [(core/hash-node node)] node))

(defn belt-depth [node]
  (if (core/has-children? node)
    (+ 1
       (apply max (map belt-depth (core/children node))))
    1)
  )

(comment
  (map belt-depth @parent-less-nodes-cache))

;; (defn belt-depth-right-most [node]
;;   (if (core/has-children? node)
;;     (+ 1
;;        (belt-depth-right-most (:core/right node)))
;;     1)
;;   )

;; defn append-leaf
;; TODO: in progress
(comment
  (if (= 0 (last (primitives.core/S-n @leaf-count)))
   ;; if 0, create new parent of right-most parent-less node - parent-less node becomes left child and new leaf becomes right child. Then merge right-most parents with equal height.
   "append and merge"
   ;; if 1, create new parent of (singleton) right-most parent-less node. Then stop.
   ;; "append only"
   (let [new-rightmost (core/mmb-append-leaf (core/belt-child-right-most core/example-belt) (core/leaf 3))]
     (core/node new-rightmost))
   ;; (assoc-in @core/storage)
   ))

(map #(nth @storage-array %) (parent-less-nodes))

(not (core/has-children? (core/belt-child-right-most core/example-belt)))
(core/node (core/belt-child-right-most core/example-belt) (core/leaf 3) (core/take-index))

(identity @core/storage)
(identity @storage-array)

(identity @leaf-count)
