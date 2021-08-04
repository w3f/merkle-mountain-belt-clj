(ns storage)

(def storage-array (atom '[]))
(def leaf-count (atom 0))

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
      (add-internal "x" (peak-location @leaf-count)))
    )
  )

(do
  (reset! storage-array '[])
  (reset! leaf-count 0)
  (println "------")
  (doall (map #(add-leaf %) (range 1 10)))
  (println @storage-array))

;; [0 0 0 1 0 2 x 3 0 4 x 5 x 6 x 7 0 8 x 9 x]

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

(map #(p-adic-order % 3) (range 1 500))

(defn parent-index [child-index]
  (+ child-index (mod child-index (int (Math/pow 2 (+ (p-adic-order 2 child-index) 2))))))

(= 10 (parent-index 7))
(= 10 (parent-index 9))

(= 12 (parent-index 6))
(= 12 (parent-index 10))

(defn leaf-location [n]
  (+ (* 2 n) 1))

(defn peak-location [n]
  (+ (* 2 n) 2))

(map leaf-location (range 100))
(map peak-location (range 100))
