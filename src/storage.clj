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

;; TODO: refactor ordering to create tuples of index & val first
(defn non-zero-entries []
  (map (juxt identity #(nth @storage-array %)) (filter #(not= 0 (nth @storage-array %)) (range (count @storage-array))))
  )

;; TODO: ditto
(defn parents []
  (map
   (juxt identity #(nth @storage-array %))
   (filter #(string? (nth @storage-array %)) (range (count @storage-array)))
   ))

(defn node-name [index]
  (nth @storage-array index))

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
(map (juxt identity children) (filter #(= "p" (nth @storage-array %)) (range (count @storage-array))))
(filter #(not= 0 (nth @storage-array %)) (range (count @storage-array)))
(map second (map (juxt identity children) (filter #(= (str "p-" %) (nth @storage-array %)) (range (count @storage-array)))))

;; confirm that all non-zero entries are covered by the tree
(=
 #{}
 (clojure.set/difference
  (into #{} (filter #(not= 0 (nth @storage-array %)) (range (count @storage-array))))
  (into #{} (flatten (map (juxt identity left-child right-child) (filter #(re-matches #"p-.*" (str (nth @storage-array %))) (range (count @storage-array))))))
  ))

;; [0 0 0 1 0 2 x 3 0 4 x 5 x 6 x 7 0 8 x 9 x]

(map #(p-adic-order % 3) (range 1 500))

(defn parent-index [child-index]
  (+ child-index (mod child-index (int (Math/pow 2 (+ (p-adic-order 2 child-index) 2))))))

;; test identification of children
(= 10 (parent-index 7))
(= 10 (parent-index 9))

;; test identification of children
(= 12 (parent-index 6))
(= 12 (parent-index 10))



