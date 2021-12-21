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
  (reset! lastP nil))

(:height (get peak-map lastP))
(identity @lastP)

(algo)
(defn algo []
  (let [
        ;; let h be hash of new leaf
        h (str @leaf-count "-hash")
        ;; create object P, set P.hash<-h, set P.height<-0, set P.left<-lastP
        P (peak-node (:hash (get @peak-map @lastP)) 0 h nil)
        ]
    (do
      ;; 1. Add step
      ;; store object P in peak map
      (swap! peak-map #(assoc % h P))
      ;; A[R*n+1]<-h
      (add-internal h (* 2 @leaf-count))

      ;; 2. Check mergeable
      ;; if lastP.height==0 then M.add(P)
      (if (= (:height (get @peak-map @lastP)) 0)
        (add-mergeable-stack (get @peak-map h)))

      ;; 3. reset lastP
      (reset! lastP h)

      ;; 4. merge if mergeable
      (if (not (zero? (count @mergeable-stack)))
        (do
          (let [Q (pop-mergeable-stack)
                Q (update Q :height inc)
                L (get @peak-map (:left Q))
                Q-old-hash (:hash Q)
                Q (assoc Q :hash (str (:hash L) "||" (:hash Q)))
                Q (assoc Q :left (:left L))]
            (swap! peak-map #(assoc % (:hash Q) Q))
            (add-internal (:hash Q) (inc (* 2 @leaf-count)))
            (if (= (:height Q) (:height (get @peak-map (:left Q))))
              (add-mergeable-stack Q))
            ;; if we've replaced the old lastP, should reset lastP to point to the new entry
            (if (= Q-old-hash @lastP)
              (reset! lastP (:hash Q)))
            ))
        )
      (swap! leaf-count inc)
      ;; 5. TODO update range nodes

      ;; show results
      [@peak-map @peak-array @mergeable-stack @lastP]))
  )

(algo)

(@peak-map)

(keys @peak-map)
