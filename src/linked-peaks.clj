(ns linked-peaks
  (:require [core]))

;; TODO: remove these entirely: they're only here for debugging now: only need hashes later
(defn internal-node [left height hash parent]
  {:left left
   :height height
   :hash hash
   :parent parent
   :type :internal})

(defn peak-node [left right height hash]
  {:left left
   :right right
   :height height
   :hash hash
   :parent nil
   :type :peak})

(defn range-node [left right hash parent]
  {:left left
   :right right
   :hash hash
   :parent parent
   :type :range})

(defn belt-node [left right hash parent]
  {:left left
   :right right
   :hash hash
   :parent parent
   :type :belt})

(def lastP (atom nil))

;; (def R-count (atom 0))

(def mergeable-stack (atom []))

(def leaf-count (atom 0))

(def node-map (atom {}))
(def node-array (atom []))

(defn pop-mergeable-stack []
  (let [pop-item (last @mergeable-stack)]
    (swap! mergeable-stack (comp #(into [] %) drop-last))
    (get @node-map pop-item)))

(defn add-mergeable-stack [item]
  (swap! mergeable-stack #(assoc % (count %) (:hash item))))

(defn add-internal [item index]
  (let [array-len (count @node-array)
        ;; incidentally correct since index is calculated starting at 1 in lieu of 0
        zero-leaves (- index array-len)]
    (swap! node-array concat (repeat zero-leaves 0) (list item))))

(defn reset-all []
 (do
   (reset! node-map {})
   (reset! node-array [])
   (reset! mergeable-stack [])
   (reset! leaf-count 0)
   (reset! lastP nil)))

(:height (get @node-map @lastP))
(identity @lastP)

(defn hop-left [node & target-map]
  (:left (get (or (first target-map) @node-map) node)))

(defn hop-parent [node & target-map]
  (:parent (get (or (first target-map) @node-map) node)))

(def pointers (atom #{}))
;; could switch to object pointers to avoid :right values, but not convinced that the advantages outweigh the disadvantages:
;; +: don't need to update left pointers of right siblings
;; -: harder links to node-array
;; -: I suspect we need right pointers for peaks anyways to facilitate range node updates
(defn get-pointer []
  (let [pointer-upper-bound 9999
        pointer (first (drop-while
                        #(contains? @pointers %)
                        (repeatedly #(rand-int pointer-upper-bound))))]
    (swap! pointers #(conj % pointer))
    pointer)
  )
(reset! pointers #{})

(comment
  (algo true))
(defn algo [upgrade?]
  (let [
        ;; let h be hash of new leaf
        ;; h (str @leaf-count "-hash")
        h #{@leaf-count}
        ;; pointer (get-pointer)
        ;; create object P, set P.hash<-h, set P.height<-0, set P.left<-lastP
        P (peak-node (:hash (get @node-map @lastP)) nil 0 h)
        ;; P (peak-node (:hash (get @node-map @lastP)) nil 0 h)
        ]
    (do
      ;; 1. Add step
      ;; store object P in peak map
      (swap! node-map #(assoc % h P))
      ;; (swap! node-map #(assoc % pointer P))
      ;; A[R*n+1]<-h
      (add-internal h (* 2 @leaf-count))

      ;; 2. Check mergeable
      ;; if lastP.height==0 then M.add(P)
      (if (and @lastP (= (:height (get @node-map @lastP)) 0))
        (add-mergeable-stack (get @node-map h)))

      ;; prelim: set right pointer of lastP to h
      ;; #dbg
      (if @lastP (swap! node-map #(assoc-in % [@lastP :right] h)))
      ;; 3. reset lastP
      (reset! lastP h)

      ;; 4. merge if mergeable
      ;; #dbg
      (if (not (zero? (count @mergeable-stack)))
        (do
          (let [Q (pop-mergeable-stack)
                Q-old Q
                ;; Q-old-hash (:hash Q-old)
                Q (update Q :height inc)
                L (get @node-map (:left Q))
                Q (assoc Q :hash (apply sorted-set (concat (:hash L) (:hash Q))))
                Q (assoc Q :left (:left L))]
            ;; add new leaf to node-map
            (swap! node-map #(assoc % (:hash Q) Q))
            ;; update :left pointer of Q-old's :right
            (if (:right Q-old)
              ;; #dbg
              (swap! node-map #(assoc-in % [(:right Q-old) :left] (:hash Q))))
            ;; update :right pointer of L's :left
            (if (:left L)
              (swap! node-map #(assoc-in % [(:left L) :right] (:hash Q))))
            ;; update new parent-values
            ;; (swap! node-map #(assoc-in % [(:hash L) :parent] (:hash Q)))
            ;; (swap! node-map #(assoc-in % [Q-old-hash :parent] (:hash Q)))

            ;; update new parent-values, change type of children to internal (also removes :right key)
            ;; TODO: simply remove these nodes - they aren't needed beyond debugging and just clutter the interface
            (swap! node-map #(assoc % (:hash L) (internal-node (:left L) (:height L) (:hash L) (:hash Q))))
            (swap! node-map #(assoc % (:hash Q-old) (internal-node (:left Q-old) (:height Q-old) (:hash Q-old) (:hash Q))))
            ;; change type of children to internal
            ;; (swap! node-map #(assoc-in % [(:hash L) :type] :internal))
            ;; (swap! node-map #(assoc-in % [Q-old-hash :type] :internal))

            (add-internal (:hash Q) (inc (* 2 @leaf-count)))
            ;; issue is that :left of Q can be outdated since may have had subsequent merge
            (if (= (:height Q) (:height (get @node-map (:left Q)))
                     )
              (if (nil? (:parent (get @node-map (:left Q))))
                (add-mergeable-stack Q)
                (throw (Exception. ":left should always be updated whenever we have a merge - can't have a parent!"))
                )
              )

            ;; if we've replaced the old lastP, should reset lastP to point to the new entry
            (if (= (:hash Q-old) @lastP)
              (reset! lastP (:hash Q)))
            ;; TODO: the following has a smarter integration

            (if (= (:hash Q-old) (hop-left @lastP))
              (throw (Exception. ":left of lastP is outdated")))
            (if (= (:hash Q-old) (:left (get @node-map (:left (get @node-map @lastP)))))
              (throw (Exception. ":left of lastP's left is outdated")))

            (if upgrade?
              (let [
                    left-most-sibling-peak (last (take-while #(and (some? %) (nil? (hop-parent %))) (iterate hop-left @lastP)))
                    correct-sibling-of-left-most (take-while some? (iterate hop-parent (hop-left left-most-sibling-peak)))
                    ]
                (if (and (some? left-most-sibling-peak) (< 1 (count correct-sibling-of-left-most)))
                  ;; (throw (Exception. "should never get :left dissociated"))
                  ;; #dbg
                  (get-in @node-map [left-most-sibling-peak :left])
                  ;; #dbg
                  (swap! node-map #(assoc-in % [left-most-sibling-peak :left] (last correct-sibling-of-left-most)))
                  )))
            )
          )
        )
      (swap! leaf-count inc)

      ;; 5. TODO update range nodes

      ;; check (difference (S-n n) (S-n (dec n)))
      ;; recalculate only those members of S-n that are in the difference set from above

      ;; show results
      ;; (clojure.pprint/pprint [@node-map @node-array @mergeable-stack @lastP])
      ;; (clojure.pprint/pprint @node-map)
      ;; (clojure.pprint/pprint @node-map)
      ))
  )

(some? (play-algo 1222 false))
(def algo-1222 (play-algo 1222 true))
(def algo-1277 (play-algo 1277 true))
(def algo-1278 (play-algo 1278 true))
(def algo-1279 (play-algo 1279 true))

;; TODO: seems :left of #{8..15} is outdated -> FIX!
(filter #(some? (:right %)) (vals (:node-map (play-algo 20 true))))
(filter #(= :peak (:type %)) (vals (:node-map (play-algo 20 true))))
(filter #(= :internal (:type %)) (vals (:node-map (play-algo 20 true))))

(defn reset-atoms-from-cached [cached]
  (reset! node-map (:node-map cached))
  (reset! node-array (:node-array cached))
  (reset! mergeable-stack (:mergeable-stack cached))
  (reset! lastP (:lastP cached))
  (reset! leaf-count (:leaf-count cached))
  )

(defn current-atom-states []
  {
   :node-map @node-map
   :node-array @node-array
   :mergeable-stack @mergeable-stack
   :leaf-count @leaf-count
   :lastP @lastP
   })

(defn oneshot-nesting-from-cached [cached]
  (do (reset-atoms-from-cached cached)
      (merge (current-atom-states) (oneshot-nesting true))))

(defn oneshot-nesting-from-fresh [n]
  (do (play-algo n true)
      (merge (current-atom-states) (oneshot-nesting true))))

;; test that caching vs fresh has same result
(let [fresh-1222 (oneshot-nesting-from-fresh 1222)
      cached-1222 (oneshot-nesting-from-cached algo-1222)]
  (= fresh-1222 cached-1222)
  )

(filter #(= :peak (:type (get (:node-map algo-1222) (nth (:node-array algo-1222) %))))
        (range (count (:node-array algo-1222))))
(comment (list 1533 1789 2301 2397 2413 2421 2429 2437 2441 2443))

;; NOTE: shifting storage left by 3 since skipping the constant offset from the beginning (always empty)
(map #(- % 3) (sort (storage/parent-less-nodes 1222)))

(defn oneshot-nesting [upgrade?]
  (let [
        ;; {:keys [node-map node-array]} (select-keys (play-algo @leaf-count upgrade?) [:node-map :node-array])
        ;; node-map (atom (:node-map algo-1222))
        ;; node-array (atom (:node-array algo-1222))
        range-nodes (atom {})
        belt-nodes (atom {})
        sorted-peaks (atom (map #(get @node-map (nth @node-array (- (first %) 3))) (storage/parent-less-nodes-sorted-height (storage/parent-less-nodes @leaf-count))))
        storage-maps {:peak node-map
                      :range range-nodes
                      :belt belt-nodes}]

    (letfn [(update-parent [parent child]
              (swap! (get storage-maps (:type child)) (fn [storage-map] (assoc-in storage-map [(:hash child) :parent] (:hash parent)))))]
      (let [
            belt-children (doall (map (fn [belt-range-count]
                                        (reduce (fn [left-child right-child]
                                                  (let [rn (range-node (:hash left-child) (:hash right-child)
                                                                       (clojure.set/union (:hash left-child) (:hash right-child)) nil)]
                                                    (doall (map
                                                            (partial update-parent rn)
                                                            [left-child right-child]))
                                                    (swap! range-nodes (fn [range-nodes] (assoc range-nodes (:hash rn) rn)))
                                                    rn
                                                    ))
                                                (take belt-range-count
                                                      (first (swap-vals! sorted-peaks (fn [current] (drop belt-range-count current)))))
                                                ))
                                      (map count (core/belt-ranges @leaf-count))))
            belts (doall
                   (reduce (fn [left-child right-child]
                             (let [bn (belt-node (:hash left-child) (:hash right-child)
                                                 (clojure.set/union (:hash left-child) (:hash right-child)) nil)]
                               (doall (map
                                       (partial update-parent bn)
                                       [left-child right-child]))
                               (swap! belt-nodes (fn [belt-nodes] (assoc belt-nodes (:hash bn) bn)))
                               bn
                               ))
                           belt-children
                           ))
            ]
        {:belt-children belt-children
         :range-nodes @range-nodes
         :belts @belt-nodes
         ;; :node-map node-map
         ;; :node-array node-array
         }))
    ))

(defonce result-1222-cached (play-algo-with-oneshot-nesting 1222 true))
(=
 (map #(if (instance? clojure.lang.Atom %) @% %) (vals result-1222-cached))
 (map #(if (instance? clojure.lang.Atom %) @% %) (vals (play-algo-with-oneshot-nesting 1222 true))))
(map some? (map (fn [val] (get @(:range-nodes result-1222) val))
                (map :hash (filter (fn [entry] (= :range (:type entry)))
                                   (map #(select-keys % [:type :hash]) (:belt-children result-1222))))))
;; -> can find all range nodes in collector

(map some? (map (fn [val] (get @(:node-map result-1222) val))
                (map :hash (filter (fn [entry] (= :range (:type entry)))
                                   ;; (map #(select-keys % [:type :hash]) (:belt-children result-1222) )))))
                                   (:belt-children result-1222)))))
;; -> can find first range node in collector

(map some? (map (fn [val] (:parent (get @(:range-nodes result-1222) val)))
                (map :hash (filter (fn [entry] (= :range (:type entry)))
                                   (map #(select-keys % [:type :hash]) (:belt-children result-1222))))))
;; ERGO -> the two last belt children don't have a daddy set, i.e. we're not updating this with final belt node? TODO: Investigate!!!

(count @(:range-nodes result-1222))
(count @(:belts result-1222))
;; ERGO -> it adds new range nodes, and the old ones don't attain parents!

(map some? (map (fn [val] (:parent (get @(:range-nodes result-1222) val)))
                (map :hash (filter (fn [entry] (= :range (:type entry)))
                                   (map #(select-keys % [:type :hash]) @(:range-nodes result-1222))))))
;; ERGO -> the two last belt children don't have a daddy set, i.e. we're not updating this with final belt node? TODO: Investigate!!!

(map some? (map (fn [val] (get @(:node-map result-1222) val))
      (map :hash (filter (fn [entry] (= :peak (:type entry))) (map #(select-keys % [:type :hash]) (:belt-children result-1222) )))))
;; -> peaks have parents set


(count @(:range-nodes result-1222))
(count @(:belts result-1222))
(get @(:range-nodes result-1222)
     (:hash (first (:belt-children result-1222))))

(map #(get (:node-map algo-1222)
           (nth (:node-array algo-1222) %))
     (map #(- % 3) (storage/parent-less-nodes 1222)))

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
                         (nth (sort (group-by count (keys @node-map))) n)))))
        (range (count (core/S-n @leaf-count))))

(def algo-new-mismatch (play-algo (inc (last-algo-match)) true))
(def algo-old-mismatch (play-algo (inc (last-algo-match)) false))

(:mergeable-stack algo-new-mismatch)
(:mergeable-stack algo-old-mismatch)
(:lastP algo-new-mismatch)
(:lastP algo-old-mismatch)
(clojure.set/difference (into #{} (:node-map algo-new-mismatch))
                        (into #{} (:node-map algo-old-mismatch)))
(clojure.set/difference (into #{} (:node-map algo-old-mismatch))
                        (into #{} (:node-map algo-new-mismatch)))

(defn play-algo [n upgrade?]
  (do (reset-all)
      (doall (repeatedly n #(algo upgrade?)))
      ;; (println "-----------------")
      ;; (clojure.pprint/pprint @node-map)
      {:node-map @node-map
       :lastP @lastP
       :mergeable-stack @mergeable-stack
       :node-array @node-array
       :leaf-count @leaf-count}
      ))

;; test that that tree construction is correct
(let [n 1222
      nodes (play-algo n true)
      parent-less (filter #(= nil (:parent (val %))) (:node-map nodes))]
  (every? true?
          [
           (= (storage/S-n n) (reverse (sort (map (comp :height val) parent-less))))
           (= (storage/S-n n) (reverse (map (comp :height #(get @node-map %)) (take-while some? (iterate hop-left (:lastP nodes))))))
           (every? nil? (map #(:parent (get @node-map %)) (take-while #(some? (get @node-map %)) (iterate hop-left @lastP))))
           ]
          ))

(defn last-algo-match
  "plays algo while the upgrade and old algo still match"
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
  (map (fn [[k v]] [k (:parent v)]) @node-map)
  (keys @node-map)
  )

(keys @node-map)
(get @node-map #{8 9})
