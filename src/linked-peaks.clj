(ns linked-peaks)

(defn peak-node [left height hash parent]
  {:left left
   :height height
   :hash hash
   :parent parent})

(defn range-node [left right hash parent]
  {:left left
   :right right
   :hash hash
   :parent parent})

(def lastP (atom (peak-node nil 0 nil nil)))

;; (def R-count (atom 0))

(def mergeable-stack (atom []))

(def leaf-count (atom 0))

(def peak-map (atom {}))
(def peak-array (atom []))

(defn pop-mergeable-stack []
  (let [pop-item (last @mergeable-stack)]
    (swap! mergeable-stack (comp #(into [] %) drop-last))
    pop-item))

(defn add-mergeable-stack [item]
  (swap! mergeable-stack #(assoc % (count %) item)))

(defn add-internal [item index]
  (let [array-len (count @peak-array)
        ;; incidentally correct since index is calculated starting at 1 in lieu of 0
        zero-leaves (- index array-len)]
    (swap! peak-array concat (repeat zero-leaves 0) (list item))))

(do
  (reset! peak-map {})
  (reset! peak-array [])
  (reset! mergeable-stack [])
  (reset! leaf-count 0)
  (reset! lastP (peak-node nil nil nil nil)))

(:height @lastP)
(identity @lastP)

(defn algo []
  (let [h (str @leaf-count "-hash")]
    (do
      ;; 1. Add step
      (swap! peak-map #(assoc % h (peak-node (:hash @lastP) 0 h nil)))
      (add-internal h (* 2 @leaf-count))
      ;; 2. Check mergeable
      (if (= (:height @lastP) 0)
        (add-mergeable-stack (get @peak-map h)))
      ;; 3. reset lastP
      (reset! lastP (get @peak-map h))
      ;; 4. merge if mergeable
      (if (not (zero? (count @mergeable-stack)))
        (do
          (let [Q (pop-mergeable-stack)
                Q (update Q :height inc)
                L (:left (:hash Q))
                Q (assoc Q :hash (str (:hash L) (:hash Q)))
                Q (assoc Q :left (:left (:hash L)))]
            (add-internal (:hash Q) (inc (* 2 @leaf-count))))))
      (swap! leaf-count inc)
      [@peak-map @mergeable-stack]))
  )

(algo)
