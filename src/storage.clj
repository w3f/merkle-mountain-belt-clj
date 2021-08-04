(ns storage)

(def storage-array (atom '[]))
(def leaf-count (atom 0))

(conj (conj @storage-array 0) 1)
(defn add-leaf-to-storage [leaf]
  (swap! storage-array (fn [sa e] (conj sa e)) leaf))

(defn add-zero-leaf []
  (add-leaf-to-storage 0))

(add-zero-leaf)
(reset! storage-array '[])
(identity @storage-array)

(defn add-leaf [leaf]
  (do
    ;; increase the leaf index
    (swap! leaf-count inc)
    (println (str "leaf-count: " @leaf-count))
    (let [array-len (count @storage-array)
         leaf-index ((add-operation @leaf-count) @leaf-count)
         additional-leaves (- leaf-index array-len)]
      (println (str "additional leaves: "additional-leaves))
     (doall (repeatedly (max 0 (dec additional-leaves)) add-zero-leaf))
     (add-leaf-to-storage leaf)
     (println (str "storage: " @storage-array))
     (println (str "storage-index: " (count @storage-array)" leaf-index: " leaf-index))
     )))

(do
  (reset! storage-array '[0])
  (reset! leaf-count 0)
  (println "------")
  (doall (map #(add-leaf %) (range 1 10)))
  (println @storage-array))

(identity @leaf-count)
(identity @storage-array)

(defn add-operation [leaf-count]
  (if (= (+ leaf-count 1) (int (Math/pow 2 (p-adic-order 2 (+ leaf-count 1)))))
    addition-step-hash-location
    merge-step-hash-location
    )
  )

(map #(add-operation %) (range 20))

(add-leaf)

(highest-exponent-st-dividing 2 8)

;; (map
;;  (fn [n] (if ((/ (Math/log (+ n 1)) (Math/log 2)))
;;           (str n ": addition")
;;           (str n ": merge")))
;;  (range 20))

(int (Math/pow 2 ##Inf))
(map (fn [n]
       (if (= (+ n 1) (int (Math/pow 2 (p-adic-order 2 (+ n 1)))))
        (str n ": addition")
        (str n ": merge")))
     (range 1 20)
     )

(let [n 2] (= n (int (Math/pow 2 (highest-exponent-st-dividing 2 n)))))

(Math/pow 2 1)
(map (partial highest-exponent-st-dividing 2) (range 10))

(defn highest-exponent-st-dividing [p n]
  (last
   (filter #(= 0.0
               (mod (Math/abs n) (Math/pow p %)))
           (range 0 (Math/abs n)))))

;; (mod 45 (Math/pow 3 2))
;; p = 3, n = 45
;; #(= 0 (mod n (Math/pow p %)))

(highest-exponent-st-dividing -45 3)
(highest-exponent-st-dividing 2 6)

(defn p-adic-order [p n]
  (if (= 0 n)
    ##Inf
    (highest-exponent-st-dividing p n)
      ))

(map #(p-adic-order % 3) (range 1 500))

(defn parent-index [child-index]
  (+ child-index (mod child-index (int (Math/pow 2 (+ (p-adic-order 2 child-index) 2))))))

(Math/pow 2 (+ (p-adic-order 2 6) 2))

(= 10 (parent-index 7))
(= 10 (parent-index 9))

(= 12 (parent-index 6))
(= 12 (parent-index 10))

(defn addition-step-hash-location [n]
  (do
    (println (str n ": addition"))
    (+ (* 2 n) 1)))

(defn merge-step-hash-location [n]
  (do
    (println (str n ": merge"))
    (+ (* 2 n) 2)))

(map addition-step-hash-location (range 100))
(map merge-step-hash-location (range 100))
