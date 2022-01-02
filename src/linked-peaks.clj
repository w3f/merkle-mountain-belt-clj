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

(defn reset-all []
 (do
   (reset! peak-map {})
   (reset! peak-array [])
   (reset! mergeable-stack [])
   (reset! leaf-count 0)
   (reset! lastP nil)))

(:height (get @peak-map @lastP))
(identity @lastP)

(defn hop-left [node]
  (:left (get @peak-map node)))

(defn hop-parent [node]
  (:parent (get @peak-map node)))

(algo)
(defn algo [upgrade?]
  (let [
        ;; let h be hash of new leaf
        ;; h (str @leaf-count "-hash")
        h #{@leaf-count}
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
                Q (assoc Q :hash (apply sorted-set (concat (:hash L) (:hash Q))))
                Q (assoc Q :left (:left L))]
            ;; add new leaf to peak-map
            (swap! peak-map #(assoc % (:hash Q) Q))
            ;; update new parent-values
            (swap! peak-map #(assoc-in % [(:hash L) :parent] (:hash Q)))
            (swap! peak-map #(assoc-in % [Q-old-hash :parent] (:hash Q)))

            (swap! peak-map #(assoc-in % [Q-old-hash :parent] (:hash Q)))

            (add-internal (:hash Q) (inc (* 2 @leaf-count)))
            ;; issue is that :left of Q can be outdated since may have had subsequent merge
            (if (= (:height Q) (:height (get @peak-map (:left Q))))
              (add-mergeable-stack Q))
            ;; if we've replaced the old lastP, should reset lastP to point to the new entry
            (if (= Q-old-hash @lastP)
              (reset! lastP (:hash Q)))
            ;; TODO: the following has a smarter integration
            (if (not upgrade?)
              (do (if (= Q-old-hash (hop-left @lastP))
                    (swap! peak-map #(assoc-in % [@lastP :left] (:hash Q))))
                  (if (= Q-old-hash (:left (get @peak-map (:left (get @peak-map @lastP)))))
                    (swap! peak-map #(assoc-in % [(:left (get @peak-map @lastP)) :left] (:hash Q)))))
              (let [
                    left-most-sibling-peak (last (take-while #(and (some? %) (nil? (hop-parent %))) (iterate hop-left @lastP)))
                    correct-sibling-of-left-most (take-while some? (iterate hop-parent (hop-left left-most-sibling-peak)))
                    ]
                (if (and (some? left-most-sibling-peak) (< 1 (count correct-sibling-of-left-most)))
                  (swap! peak-map #(assoc-in % [left-most-sibling-peak :left] (last correct-sibling-of-left-most))))))
            )
          )
        )
      (swap! leaf-count inc)
      ;; 5. TODO update range nodes

      ;; show results
      ;; (clojure.pprint/pprint [@peak-map @peak-array @mergeable-stack @lastP])
      ;; (clojure.pprint/pprint @peak-map)
      ;; (clojure.pprint/pprint @peak-map)
      ))
  )

(algo true)
(play-algo (last-algo-match) true)
(first-algo-mismatch)

(play-algo 300 false)

;; check that all peaks have correct children
(every? (fn [n]
          (eval `(and ~@((juxt
                          ;; number of leaves with this height
                          ;; (comp count second)
                          ;; does every leaf "hash" start with a 2^height
                          (comp (fn [list-first-indices] (every? #(= 0.0 %) (map #(mod % (Math/pow 2 n)) list-first-indices))) sort #(map first %) second)
                          ;; is every leaf "hash" a range from the first index until first index + 2^height?
                          (comp (fn [child-list] (every? #(= % (into #{} (range (first %) (+ (first %) (Math/pow 2 n))))) child-list)) second)
                          ;; identity
                          )
                         ;; group the peaks by hashset length
                         (nth (sort (group-by count (keys @peak-map))) n)))))
        (range (count (core/S-n @leaf-count))))

(def algo-new-mismatch (play-algo (inc (last-algo-match)) true))
(def algo-old-mismatch (play-algo (inc (last-algo-match)) false))

(:mergeable-stack algo-new-mismatch)
(:mergeable-stack algo-old-mismatch)
(:lastP algo-new-mismatch)
(:lastP algo-old-mismatch)
(clojure.set/difference (into #{} (:peak-map algo-new-mismatch))
                        (into #{} (:peak-map algo-old-mismatch)))
(clojure.set/difference (into #{} (:peak-map algo-old-mismatch))
                        (into #{} (:peak-map algo-new-mismatch)))
(@peak-map)

(defn play-algo [n upgrade?]
  (do (reset-all)
      (doall (repeatedly n #(algo upgrade?)))
      ;; (println "-----------------")
      ;; (clojure.pprint/pprint @peak-map)
      {:peak-map @peak-map
       :lastP @lastP
       :mergeable-stack @mergeable-stack}
      ))

;; test that that tree construction is correct
(let [n 1222
      nodes (play-algo n true)
      parent-less (filter #(= nil (:parent (val %))) (:peak-map nodes))]
  (= (reverse (sort (map (comp :height val) parent-less))) (storage/S-n n)))
(every? nil? (map #(:parent (get @peak-map %)) (take-while #(some? (get @peak-map %)) (iterate hop-left @lastP))))

(defn last-algo-match
  "plays algo while the upgrade and old algos still match"
  []
  (last (take-while
    #(let [
           non-upgrade (play-algo % false)
           upgrade (play-algo % true)
           ]
       (= non-upgrade upgrade))
    (range 300))))

(defn first-algo-mismatch
  "plays algo until first mismatch and returns the differences"
  []
  (let [first-mismatch (inc (last-algo-match))]
    (println "-----------------")
    (clojure.pprint/pprint
     {:first-mismatch first-mismatch
      :old (play-algo first-mismatch false)
      :new (play-algo first-mismatch true) })))

(do
  ;; (play-algo 10 false)
  (play-algo 0 false)
  (map (fn [[k v]] [k (:parent v)]) @peak-map)
  (keys @peak-map)
  )

(keys @peak-map)
(get @peak-map #{8 9})
