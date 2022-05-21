(ns storage
  (:require [primitives.core]
            [primitives.storage :refer [left-child right-child storage-array leaf-location peak-location children node-height-literal parent-index parent-less-nodes position-parentless-nodes highest-exponent-st-dividing parent-less-nodes-atom parent-less-nodes-cache node-name range-node-edges-reduced range-node-edges ]]
            ))

(defonce peaks-accumulator (atom []))

(comment
  (aget (bytes (byte-array (byte 4))) 1)
  (bit-and 1 1))

;; (children 20)

(primitives.core/binary-repr-of-n 14)

;; (defn is-left-child [node]
;;   (if (node)))

(defn add-internal [item index]
  (let [array-len (count @storage-array)
        ;; incidentally correct since index is calculated starting at 1 in lieu of 0
        zero-leaves (- index array-len)]
    (swap! storage-array concat (repeat zero-leaves 0) (list item))
    ))

(defn add-leaf [leaf]
  (do
    ;; increase the leaf index
    (swap! primitives.storage/leaf-count inc)
    (add-internal leaf (leaf-location @primitives.storage/leaf-count))
    (swap! parent-less-nodes-atom #(conj % (leaf-location @primitives.storage/leaf-count)))
    (if
        (not= (+ @primitives.storage/leaf-count 1) (int (Math/pow 2 (primitives.storage/p-adic-order 2 (+ @primitives.storage/leaf-count 1)))))
      (do
        (add-internal (str "p-" (swap! primitives.storage/node-count inc)) (peak-location @primitives.storage/leaf-count))
        (swap! parent-less-nodes-atom #(conj % (peak-location @primitives.storage/leaf-count)))
        (swap! parent-less-nodes-atom #(apply disj % (children (peak-location @primitives.storage/leaf-count)))))
      )
    (swap! peaks-accumulator #(conj % @parent-less-nodes-atom)))
  )

(comment
  (= (primitives.core/S-n @primitives.storage/leaf-count)
    (reverse (sort (map node-height-literal @parent-less-nodes-cache)))))

(map (juxt identity node-height-literal) @parent-less-nodes-cache)

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
   (reset! primitives.storage/leaf-count 0)
   (reset! primitives.storage/node-count 0)
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
(run 20)

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

(apply max (map #(double (- (parent-index %) %)) (range 1 1000 2)))
(apply max (map #(double (/ (parent-index %) %)) (range 1 1000 2)))
(map #(primitives.storage/p-adic-order 2 %) (range 1 1000 2))
;; (map #(/ % parent-index) (range 1 500 2))

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

;; (bag-left-to-right (map #(primitives.core/leaf %) (primitives.core/S-n 7)))

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

;; verify that the number of peaks == (dec binary length of primitives.storage/leaf-count)
(= (dec (count (primitives.core/binary-repr-of-n @primitives.storage/leaf-count)))
   (count @parent-less-nodes-cache))

;; see https://hackmd.io/4k2wjlWfTVqgW0Mp4bLSSQ?view#An-initial-description-without-bagging
;; to get mountain heights
;; (defn s-i )

(identity @parent-less-nodes-cache)

(defn find-right-most-parentless [])

;; (identity @core/storage)

(defn belt-depth [node]
  (if (primitives.core/has-children? node)
    (+ 1
       (apply max (map belt-depth (primitives.core/children node))))
    1)
  )

(comment
  (map belt-depth @parent-less-nodes-cache))

;; (defn belt-depth-right-most [node]
;;   (if (primitives.core/has-children? node)
;;     (+ 1
;;        (belt-depth-right-most (:core/right node)))
;;     1)
;;   )

;; defn append-leaf
;; TODO: in progress
(comment
  (if (= 0 (last (primitives.core/S-n @primitives.storage/leaf-count)))
   ;; if 0, create new parent of right-most parent-less node - parent-less node becomes left child and new leaf becomes right child. Then merge right-most parents with equal height.
   "append and merge"
   ;; if 1, create new parent of (singleton) right-most parent-less node. Then stop.
   ;; "append only"
   (let [new-rightmost (primitives.core/mmb-append-leaf (primitives.core/belt-child-right-most core/example-belt) (primitives.core/leaf 3))]
     (primitives.core/node new-rightmost))
   ;; (assoc-in @core/storage)
   ))

(map #(nth @storage-array %) (parent-less-nodes))

(comment
  (not (primitives.core/has-children? (primitives.core/belt-child-right-most core/example-belt)))
  (primitives.core/node (primitives.core/belt-child-right-most core/example-belt) (primitives.core/leaf 3) (primitives.core/take-index)))
