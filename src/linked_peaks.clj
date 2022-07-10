(ns linked-peaks
  (:require
   [clj-async-profiler.core :as prof]
   [clojure.java.io]
   [clojure.pprint]
   [clojure.set]
   [clojure.string]
   [clojure.test]
   [clojure.walk]
   [primitives.core]
   [primitives.storage]
   [state
    :refer
    [belt-nodes
     current-atom-states
     lastP
     leaf-count
     mergeable-stack
     node-array
     node-map
     pointers
     range-nodes
     root-belt-node]]))

(println "start:" (new java.util.Date))

(def run-tests (atom false))
(defn toggle-tests [] (swap! run-tests #(not %)))

;; generic tooling
(def global-debugging (atom false))
(defn toggle-debugging [] (swap! global-debugging #(not %)))
(comment (toggle-debugging))
(def debugging-flags (atom #{:singleton-range :merge :belt-merge :range-merge-replace :range-phantom}))
(defn all-debugging []
  (reset! debugging-flags #{:singleton-range :merge :belt-merge :range-merge-replace :peak-merge}))
(defn set-debugging-flags [flags]
  (reset! debugging-flags (into #{} flags)))
(defn debugging [flags]
  (and @global-debugging
       (every? #(contains? @debugging-flags %) flags)))

(defn truncate-#set-display [data]
  (clojure.walk/postwalk
   #(if (and (contains? #{clojure.lang.PersistentHashSet
                          clojure.lang.PersistentTreeSet} (type %))
             (every? number? %))
      (if (< 1 (count %))
        (str "#{" (apply min %) ".." (apply max %) "}")
        (str %))
      %)
   ;; (sort-by #(apply min (:hash %)) data)
   data))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(comment
  "example usage"
  (truncate-#set-display (:belt-nodes (oneshot-nesting-from-fresh 8 true)))
  (truncate-#set-display (:range-nodes (oneshot-nesting-from-fresh 8 true))))

(defn display-type-filtered [data type]
  (truncate-#set-display
   (filter #(= type (:type %)) data)))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(comment
  "example usage"
  (display-type-filtered (vals (:node-map (play-algo 1222 true))) :peak)
  (display-type-filtered (vals (:node-map (play-algo 20 true))) :internal))

(defn merge-rule [n]
  (let [b (primitives.core/binary-repr-of-n (inc n))
        j (primitives.storage/p-adic-order 2 (inc n))
        m1 (if (< 1 j) "new leaf joins last range"
               (get {0 "new leaf participates in merge"
                     1 "new leaf forms a range alone"} j))
        [bj+2 bj+1 bj] (if (< (+ j 2) (count b))
                         (map #(Integer/parseInt (str %)) (map #(nth (reverse b) %) [(+ j 2) (+ j 1) j]))
                         (throw (Exception. "bitlength should exceed 2-adic-order by at least 2")))
        m2 (if (= [bj+1 bj] [0 1])
             "M.M. not alone in range"
             (if (= [bj+2 bj+1 bj] [0 1 1])
               "M.M. alone in range"
               (if (= [bj+2 bj+1 bj] [1 1 1])
                 "M.M. joins prev range"
                 (throw (Exception. (clojure.string/join " " ["unhandled bitsequence:" "[" bj+2 bj+1 bj "]"]))))))]

    [m1 m2]))

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
   ;; peak nodes are instantiated without a parent
   :parent nil
   :type :peak})

;; range nodes don't track height
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
(def nil-leaf (internal-node nil ##Inf #{} #{}))

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
    (swap! node-array (fn [coll items]
                        (reduce
                         conj coll items)) (concat (repeat zero-leaves 0) (list item)))))

(defn reset-all []
  ;; NOTE: need to already set parent for phantom node, range, and belt
  (reset! node-map {#{} (assoc (peak-node nil nil ##Inf #{}) :parent #{})})
  (reset! node-array [])
  (reset! mergeable-stack [])
  (reset! leaf-count 0)
  (reset! lastP #{})
  (reset! belt-nodes {#{} (belt-node nil #{} #{} #{0})})
  (reset! root-belt-node #{})
  (reset! range-nodes {#{} (range-node nil #{} #{} #{})}))

(defn hop-left [node & target-map]
  (:left (get (or (first target-map) @node-map) node)))

(defn hop-parent [node & target-map]
  (:parent (get (or (first target-map) @node-map) node)))

(comment (count #_{:clj-kondo/ignore [:unresolved-symbol]}
          (oneshot-nesting-from-fresh 1223 true)))
(comment (count #_{:clj-kondo/ignore [:unresolved-symbol]}
          (play-algo 1223 true)))
(comment (keys (:node-map #_{:clj-kondo/ignore [:unresolved-symbol]}
                (play-algo 6 true))))

(comment
  (count (merge
          #_{:clj-kondo/ignore [:unresolved-symbol]}
          (oneshot-nesting-from-fresh 1222)
          {:mergeable-stack (atom @mergeable-stack)
           :leaf-count (atom @leaf-count)
           :lastP (atom @lastP)})))

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
    pointer))
(reset! pointers #{})

(defn sanity-checks [Q-old]
  #_{:clj-kondo/ignore [:missing-else-branch]}
  (if (= (:hash Q-old) (hop-left @lastP))
    (throw (Exception. ":left of lastP is outdated")))
  #_{:clj-kondo/ignore [:missing-else-branch]}
  (if (= (:hash Q-old) (:left (get @node-map (:left (get @node-map @lastP)))))
    (throw (Exception. ":left of lastP's left is outdated")))
  (let [left-most-sibling-peak (last (take-while #(and (some? %) (not (contains? #{:internal :peak} (:type (get @node-map (hop-parent %)))))) (iterate hop-left @lastP)))
        correct-sibling-of-left-most (take-while #(and (some? %) (contains? #{:internal :peak} (:type (get @node-map %)))) (iterate hop-parent (hop-left left-most-sibling-peak)))]
    #_{:clj-kondo/ignore [:missing-else-branch]}
    (if (and (some? left-most-sibling-peak) (< 1 (count correct-sibling-of-left-most)))
      (throw (Exception. "should never get :left dissociated"))
      ;; #dbg
      ;; (swap! node-map #(assoc-in % [left-most-sibling-peak :left] (last correct-sibling-of-left-most)))
      )))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(comment (:range-nodes (play-algo 5 true))
         (oneshot-nesting-from-fresh 8 true)
         (algo false)
         (oneshot-nesting true))

(def storage-maps {:internal node-map
                   :peak node-map
                   :range range-nodes
                   :belt belt-nodes})

;; (truncate-#set-display (:belt-nodes (oneshot-nesting-from-fresh 8 true)))
;; (truncate-#set-display (:range-nodes (oneshot-nesting-from-fresh 8 true)))
;; (truncate-#set-display (:range-nodes (oneshot-nesting-from-fresh 8 true)))
;; (truncate-#set-display (:range-nodes (play-algo-optimized 8)))
;; (truncate-#set-display (:range-nodes (play-algo-oneshot-end 8)))

(comment (map #(get @node-map (nth @node-array (- (first %) 0))) (primitives.storage/parent-less-nodes-sorted-height (primitives.storage/parent-less-nodes @leaf-count))))

(defn oneshot-nesting
  "performs a oneshot nesting of ephemeral range and belt nodes. takes flag `singleton-ranges?` to specify whether singleton peaks should also have a range node above them"
  [singleton-ranges?]
  (let [;; {:keys [node-map node-array]} (select-keys (play-algo @leaf-count upgrade?) [:node-map :node-array])
        ;; node-map (atom (:node-map algo-1222))
        ;; node-array (atom (:node-array algo-1222))
        ;; range-nodes (atom {})
        ;; belt-nodes (atom {})
        original-sorted-peaks (map #(get @node-map (nth @node-array (first %))) (primitives.storage/parent-less-nodes-sorted-height (primitives.storage/parent-less-nodes @leaf-count)))
        ;; prepend nil as a peak to facilitate a linked list of peaks. TODO: abuse this as a pointer for the left-most peak ^^
        sorted-peaks (atom (if singleton-ranges? (cons (peak-node nil (:hash (first original-sorted-peaks)) ##Inf #{}) original-sorted-peaks) original-sorted-peaks))
        ;; sorted-peaks (atom (if singleton-ranges? (cons (peak-node #{} (:hash (first original-sorted-peaks)) ##Inf #{}) original-sorted-peaks) original-sorted-peaks))
        ]
    (reset! range-nodes {})
    (reset! belt-nodes {})
    (letfn [;; takes type of child to find its storage map, and then updates its parent
            (update-parent [parent child]
              (if (:type child) (swap! (get storage-maps (:type child)) (fn [storage-map] (assoc-in storage-map [(:hash child) :parent] (:hash parent))))
                  ;; (println child "does not have a type key!")
                  ))]
      ;; #dbg
      (let [belt-children (doall (map (fn [belt-range-count]
                                        (reduce (fn [left-child right-child]
                                                  (let [left-most (:intruder left-child)
                                                        rn (range-node (:hash left-child) (:hash right-child)
                                                                       ;; NOTE: ugly hack to use (:hash right-child) first since
                                                                       ;; (= nil (clojure.set/union nil #{}))
                                                                       ;; but
                                                                       ;; (= #{} (clojure.set/union #{} nil))
                                                                       (clojure.set/union (:hash right-child) #_{:clj-kondo/ignore [:missing-else-branch]}
                                                                                          (if-not (and singleton-ranges? left-most) (:hash left-child)))
                                                                       ;; (clojure.set/union (if-not (and singleton-ranges? left-most) (:hash left-child)) (:hash right-child))
                                                                       nil)]
                                                    (doall (map
                                                            (partial update-parent rn)
                                                            (if (and singleton-ranges? left-most) [right-child] [left-child right-child])))
                                                    (swap! range-nodes (fn [range-nodes] (assoc range-nodes (:hash rn) rn)))
                                                    rn))
                                                ;; returns all peaks that are in the given range.
                                                ;; for every iteration, include the last node from the prior range, to make a linked list of all range nodes.
                                                (update (into [] (if singleton-ranges?
                                                                   (let [[dropped remainder] (split-at (inc belt-range-count) @sorted-peaks)
                                                                         new-leader (apply clojure.set/union (map :hash (rest dropped)))]
                                                                     ;; NOTE: since sorted-peaks is never read again after last step, the (if (empty? remainder) ..) check is in fact superfluous, but putting it in nonetheless, in case this features as a bug later
                                                                     (reset! sorted-peaks (if (empty? remainder) remainder (cons {:hash new-leader} remainder)))
                                                                     (if (< 1 (count dropped))
                                                                       dropped
                                                                       (conj dropped {})))
                                                                   (take belt-range-count
                                                                         (first (swap-vals! sorted-peaks (fn [current] (drop belt-range-count current)))))))
                                                        ;; DONE: first value shouldn't be last peak, but the actual range node's hash, i.e. the concatenation of hashes of the entire range
                                                        ;; tags the first node as NOT being in the same range
                                                        0 #(if singleton-ranges? (assoc % :intruder true) %))))
                                      ;; returns number of nodes in each range
                                      (map count (cons [] (primitives.core/belt-ranges @leaf-count)))))
            ;; belt-children ()
            root-bn (doall
                     (reduce (fn [left-child right-child]
                               (let [bn (belt-node (:hash left-child) (:hash right-child)
                                                   (clojure.set/union (or (:hash left-child) #{}) (:hash right-child)) nil)]
                                 (doall (map
                                         (partial update-parent bn)
                                         [left-child right-child]))
                                 (swap! belt-nodes (fn [belt-nodes] (assoc belt-nodes (:hash bn) bn)))
                                 bn))
                             ;; TODO: ugly hack - integrate neater eventually (but not strictly necessary since oneshot algorithm is not intended for production - only to verify construction of incremental algorithm)
                             (cons {:hash nil} belt-children)))]

        (reset! root-belt-node (:hash root-bn))
        {:belt-children belt-children
         :range-nodes @range-nodes
         :belt-nodes @belt-nodes
         :root-belt-node @root-belt-node
         ;; :node-map node-map
         ;; :node-array node-array
         }))))

(map-indexed #(identity [%1 (count (cons [] (primitives.core/belt-ranges %2)))]) (range 100))

(apply > (map (comp count primitives.core/belt-ranges) [@leaf-count (inc @leaf-count)]))
(apply > (map (comp count primitives.core/belt-ranges) [12 13]))
(apply > (map (comp count primitives.core/belt-ranges) [14 15]))

(defn distinct-ranges? [M M']
  (or (= 2 (- (:height M) (:height M')))
      (contains? (into #{} @mergeable-stack) (:hash M))
      ;; TODO: might be able to remove the following if/once have unified rules independent of singleton-ness of new leaf
      (nil? (:hash M))
      (= #{} (:hash M))))

;; (distinct-ranges? (get @node-map (:left (get @node-map @lastP))) (get @node-map @lastP))
(get @node-map (:left (get @node-map @lastP)))
(get @node-map @lastP)

;; (:belt-nodes (play-algo-manual-end 13))
(comment (:node-map #_{:clj-kondo/ignore [:unresolved-symbol]}
          (play-algo 200 false)))
(comment (toggle-debugging))
(reset! debugging-flags #{:belt :merge})
(debugging [:belt])
;; (let [n 6]
;;   (= (:belt-nodes (play-algo-manual-end n))
;;      (:belt-nodes (play-algo n true))))

;; (:range-nodes (play-algo-manual-end 1))
;; (:range-nodes (play-algo 1 true))

;; (:root-belt-node (play-algo 3 false))
;; (:root-belt-node (play-algo 3 true))

(def type-rank
  "used for classifying possible parent/child types"
  (zipmap
   [:internal :peak :range :belt]
   (range)))

(defn higher-type-ranks [type]
  (filter #(< (get type-rank type) (get type-rank %)) (keys type-rank)))

(comment
  (higher-type-ranks :peak)
  ; => (:range :belt)
  )

(defn parent-type-contenders
  [type]
  (let [rank (get type-rank type)]
    (if (= rank (:peak type-rank))
      (list (get (clojure.set/map-invert type-rank) (inc rank)))
      (if (= rank (:belt type-rank))
        (list :belt)
        (list type (get (clojure.set/map-invert type-rank) (inc rank)))))))

;; Test that child types match expected result
(every? (fn [[type expected-parent-type]]
          (= expected-parent-type (parent-type-contenders type)))
        [[:internal [:internal :peak]]
         [:peak [:range]]
         [:range [:range :belt]]
         [:belt [:belt]]])

;; DONE: can simplify if use phantom belt node since
;; then have left child always have same type as parent
(defn child-type
  ([type]
   (child-type type nil))
  ([type child-leg]
   (let [rank (get type-rank type)]
     (if (<= rank (:peak type-rank))
       :internal
       (if (= child-leg :left)
         type
         (if (= child-leg :right)
           (get (clojure.set/map-invert type-rank) (dec rank))
           (throw (Exception. "no child-leg specified"))))))))

;; Test that child types match expected result
(every? (fn [[[type child-leg] expected-child-type]]
          (= expected-child-type (child-type type child-leg)))
        [[[:internal nil] :internal]
         [[:peak nil] :internal]
         [[:range :left] :range]
         [[:range :right] :peak]
         [[:belt :left] :belt]
         [[:belt :right] :range]])

;; DONE: refactor this since logic is somewhat clunky
(defn get-parent
  ([child]
   (let [parent-type-contenders (parent-type-contenders (:type child))]
     (first (filter #(and % (not= child %)) (map #(get @% (:parent child)) (vals (select-keys storage-maps parent-type-contenders)))))))
  ([child expected-parent]
   (let [parent (get-parent child)]
     (if (= expected-parent (:type parent))
       parent
       (throw (Exception. (str "parent type expected: " expected-parent "\nactual parent type: " (:type parent) " " (:hash child) " " (:type child))))))))

(comment
  (truncate-#set-display (get-parent (get @range-nodes #{96 97}))))

(defn get-child [parent child-leg]
  (let [child-type (child-type (:type parent) child-leg)]
    (second (find @(get storage-maps child-type) (get parent child-leg)))))

(defn get-sibling [entry]
  (let [parent (get-parent entry)]
    (if (= (:hash entry) (:left parent))
      (get-child parent :right)
      (get-child parent :left))))

(comment
  (get-sibling (get @node-map #{60})))

(defn new-leaf-range [oneshot-nesting? h P]
  #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:singleton-range]))}
  ;; DONE: if distinct ranges, we're also adding a new belt node for the new leaf
   (if (distinct-ranges? (get @node-map @lastP) P)
     (do
       ;; create new root belt node with new leaf's parent range node as right child and former root node as left child
       (let [new-belt-root (clojure.set/union h @root-belt-node)]
         (swap! belt-nodes #(assoc % new-belt-root (belt-node @root-belt-node h new-belt-root nil)))
         ;; TODO: skipping #{} because don't have phantom #{} belt node yet -> fix once added
         #_{:clj-kondo/ignore [:missing-else-branch]}
         (if (not= #{} @root-belt-node) (swap! belt-nodes #(assoc-in % [@root-belt-node :parent] new-belt-root)))
         ;; (swap! belt-nodes #(assoc-in % [@root-belt-node :parent] new-belt-root))
         (reset! root-belt-node new-belt-root)
         #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
          (swap! range-nodes #(assoc % h (range-node (:parent (get @node-map @lastP)) h h new-belt-root))))

       (swap! node-map #(assoc-in % [h :parent] h))
       ;; TODO: conditional here is a temporary hack since I don't wanna bother with implementing correct logic yet
       #_{:clj-kondo/ignore [:missing-else-branch]}
       (if (>= @leaf-count 8)
         (let [last-range-node-hash (:parent (get @node-map @lastP))
               last-belt-node-hash (:parent (get @range-nodes last-range-node-hash))
               new-belt-hash (clojure.set/union (or last-belt-node-hash last-range-node-hash) h)]
           (swap! belt-nodes #(assoc % new-belt-hash (belt-node (or last-belt-node-hash last-range-node-hash) h new-belt-hash nil))))))
     ;; else new leaf joins last range, i.e. get new range node above new leaf
     ;; TODO: update parent belt node hash, likewise for its left sibling
     (let [last-range (get-parent (get @node-map @lastP) :range)
           old-belt-parent (get @belt-nodes (:parent last-range))
           hash-new-range (clojure.set/union (:hash last-range) h)
           new-belt-parent (clojure.set/union hash-new-range (:left old-belt-parent))
           new-range (range-node (:hash last-range) h hash-new-range new-belt-parent)]
       #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
        (swap! range-nodes #(assoc % (:hash new-range) new-range))
       (swap! node-map #(assoc-in % [h :parent] (:hash new-range)))
       #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
        (swap! range-nodes #(assoc-in % [(:hash last-range) :parent] (:hash new-range)))
       ;; update former range-leader's parent belt to have new range-leader as child
       ;; TODO: following is wrong since old belt node is deleted
       (comment (swap! belt-nodes #(assoc-in % [(:parent new-range) :right] (:hash new-range)))
                ;; recalculate belt-node hash since has new right child
                ;; TODO: delay until last possible moment since left child may be updated too during merge
                (swap! belt-nodes #(assoc-in % [(:parent new-range) :hash] (clojure.set/union (:left belt-parent) (:hash new-range)))))

       ;; TODO: assert that old-belt-node is root belt node
       (swap! belt-nodes #(assoc % new-belt-parent (belt-node (:left old-belt-parent) (:hash new-range) new-belt-parent (:parent old-belt-parent))))
       ;; update old belt node's left parent pointer to refer to new belt node
       (if (contains? @belt-nodes (:left old-belt-parent))
         (swap! belt-nodes #(assoc-in % [(:left old-belt-parent) :parent] new-belt-parent))
         (if (contains? @range-nodes (:left old-belt-parent))
           #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
            (swap! range-nodes #(assoc-in % [(:left old-belt-parent) :parent] new-belt-parent))
           (throw (Exception. (str "old belt node's left child was invalid at leaf count " @leaf-count)))))
       (swap! belt-nodes #(dissoc % (:hash old-belt-parent)))
       (reset! root-belt-node new-belt-parent)
       ;; TODO: update siblings around update
       )))

(defn types
  "returns all types that a given hash has an entry for"
  [hash]
  (into #{} (map :type (filter some? (map #(get @% hash) [node-map range-nodes belt-nodes])))))

(defn get-node
  "returns the node with a given hash, as long as it exists for the provided type"
  [hash type]
  (get @(get storage-maps type) hash))

;; siblings of ephemeral nodes are also ephemeral
(defn co-path-ephemeral
  ([entry accumulator]
   (if (:parent entry)
     (co-path-ephemeral (get-parent entry) (if (= (:hash entry) (:parent entry))
                                             accumulator
                                             (concat accumulator [(:hash (get-sibling entry))])))
     accumulator)
   ;; (concat [(:hash (get-sibling entry))] (if (:parent entry) (co-path-ephemeral (get-parent entry))))
   ))

(comment
  (truncate-#set-display (get-parent (get-parent (get-parent (get-parent (get @node-map (:right (get @node-map #{})))))))))

;; (some? (play-algo 100 false))
(co-path-ephemeral (get @node-map (:right (get @node-map #{}))) [])

(defn sibling-index [n-index]
  (first (filter #(not= n-index %) (primitives.storage/children (primitives.storage/parent-index n-index)))))

;; (defn get-sibling-storage [])
;; (primitives.storage/children (primitives.storage/parent-index (primitives.storage/parent-index (primitives.storage/leaf-location 59))))

(comment
  (nth @node-array (sibling-index (primitives.storage/leaf-location 59)))
  (nth @node-array (sibling-index 59)))

(let [n 60]
  (primitives.storage/children (primitives.storage/parent-index (+ (* 2 n) 4))))

(defn co-path-internal
  ([index accumulator]
   (if (< (primitives.storage/parent-index index) (count @node-array))
     (co-path-internal (primitives.storage/parent-index index) (concat accumulator [(nth @node-array (sibling-index index))]))
     ;; #dbg
     ;; #dbg
     (co-path-ephemeral (get @node-map (nth @node-array index)) (let [sibling-index (sibling-index index)]
                                                                  (if (< sibling-index (count @node-array))
                                                                    ;; #dbg
                                                                    (concat accumulator [(nth @node-array sibling-index)])
                                                                    accumulator)
                                                                  accumulator
                                                                  ;; (concat accumulator [(nth @node-array sibling-index)])
                                                                  )))))

;; check that co-path is correct: ensure that we have no intersection of any of the "hashes" in the co-path
(map
 (fn [leaf-number]
   (let [co-path (co-path-internal (primitives.storage/leaf-location leaf-number) [])]
     (=
      (reduce (fn [acc v] (+ acc (count v))) 0 co-path)
      (count (into #{} (apply concat co-path))))))
 (range 1 @leaf-count))

(defn membership-proof [leaf state]
  (state/reset-atoms-from-cached! state)
  (let [leaf-index (primitives.storage/leaf-location leaf)]
    {:leaf (nth @node-array leaf-index) :co-path (co-path-internal leaf-index [])}))

(comment
  #_{:clj-kondo/ignore [:unresolved-symbol]}
  (play-algo 20 false)
  (membership-proof 10 (state/current-atom-states)))

(defn verify-membership [membership-proof root-belt-node]
  (= (reduce (fn [acc v] (if (= #{} (clojure.set/intersection acc v))
                           (clojure.set/union acc v)
                           ;; TODO: replace exception with returning eqvlt of result
                           (throw (Exception. "invalid membership proof"))))
             (:leaf membership-proof)
             (:co-path membership-proof))
     root-belt-node))

;; verify that membership proof for all leafs are correct
#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (let [state (state/current-atom-states)]
    (empty?
     (filter
      #(not (verify-membership
             (membership-proof % state)
             (:root-belt-node state)))
      (range 1 101)))))

;; verify that membership proof for incorrect state root fails
#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (let [state (state/current-atom-states)]
    (not
     (verify-membership
      (membership-proof 59 state)
      (clojure.set/union (:root-belt-node state) #{100})))))

#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  ((fn [leaf-number]
     (let [co-path (co-path-internal (primitives.storage/leaf-location leaf-number) [])]
       [(reduce (fn [acc v] (+ acc (count v))) 0 co-path)
        (count (into #{} (apply concat co-path)))]))
   65))

(defn edges-to-root
  ([ephemeral-node]
   (edges-to-root ephemeral-node []))
  ([ephemeral-node accumulator]
   (let [parent (get-parent ephemeral-node)]
     (if parent
       ;; (edges-to-root parent (concat accumulator [[(select-keys ephemeral-node [:hash :type :height]) (select-keys parent [:hash :type])]]))
       (edges-to-root parent (concat accumulator [[(str (:type ephemeral-node) ": " (truncate-#set-display (:hash ephemeral-node)))
                                                   (str (:type parent) ": " (truncate-#set-display (:hash parent)))]]))
       accumulator))))

(comment
  (get-parent (get @belt-nodes @root-belt-node)))

(defn peak-merge [oneshot-nesting?]
  ;; #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:peak-merge]))}
  ;; TODO: consider moving all conditionals into the execution logic of `algo`
  (if (not (zero? (count @mergeable-stack)))
    (let [Q (atom (pop-mergeable-stack))
          Q-old @Q
          ;; Q-old-hash (:hash Q-old)
          L (get @node-map (:left @Q))]

      (swap! Q #(update % :height inc))
      (swap! Q #(assoc % :hash (apply sorted-set (concat (:hash L) (:hash Q-old)))))
      (swap! Q #(assoc % :left (:left L)))

      ;; Q and L (should) have a preexisting parent, either a range or a belt node
      ;; #dbg ^{:break/when (not oneshot-nesting?)}
      (if (and (not oneshot-nesting?) (:parent Q-old))
        ;; #dbg
        ;; just another check to ensure that we're merging
        (if (= (:parent Q-old)
               (:parent L))
          ;; then
          (let [;; check where parent lives: should only exist in one of the maps
                parent (get-parent Q-old :range)]
            (swap! Q #(assoc % :parent (:parent parent)))
            #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
             (swap! range-nodes #(dissoc % (:hash parent))))
          ;; else
          ;; DONE (should remove): (throw (Exception. (str "parents don't match @ leaf count " @leaf-count)))
          ;; introduce more complicated algorithm: if parents don't match, still valid if their parents are not inside node-map
          ;; DONE
          ;; already know that they're distinct
          ;; check whether the parents are sibling range nodes
          ;; TODO: first condition superfluous given second
          ;; TODO: jump up chain of parents. once parent is belt node, also jump to child, then to its right sibling, then to its parent (range node in other range), and update its left pointer (doesn't change hash since still in distinct ranges)
          ;; #dbg
          (if (and (every? #(contains? @range-nodes %) [(:parent Q-old) (:parent L)])
                   (= (:parent Q-old) (:parent (get-parent L :range)))
                   (= (:parent L) (:left (get-parent Q-old :range))))
            ;; #dbg

            ;; #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:merge]))}
            (let [parent-L (get-parent L :range)
                  parent-Q-old (get-parent Q-old :range)
                  ;; this is the range node that will replace their former parent range nodes
                  ;; if the range node above former left is not leftmost node, include its left in the hash (otherwise, its left is in another range)
                  ;; DONE: update the nodes referred to to be left: left, right: newly merged peak
                  ;; TODO: should kill old parent range node that's no longer applicable
                  distinct-ranges (distinct-ranges? (get @node-map (:left @Q)) @Q)
                  rn (clojure.set/union #_{:clj-kondo/ignore [:missing-else-branch]}
                      (if (not distinct-ranges)
                        (:left parent-L))
                                        (:hash @Q))
                  ;; DONE (fixed above): the following currently only *preserves* range splits - should check whether the two range nodes should now be in the same range
                  ;; rn (clojure.set/union (if (not= (:hash parent-L) (:right parent-L)) (:left parent-L)) (:hash @Q))
                  ;; Q-old is a peak node, so its immediate parent is certainly a range node. The only unknown is the type of the parent's parent
                  grandparent-type (if (and (not= (:parent parent-Q-old) (:hash parent-Q-old))
                                            (contains? @range-nodes (:parent parent-Q-old)))
                                     :range
                                     (if (contains? @belt-nodes (:parent parent-Q-old))
                                       :belt
                                       ;; (throw (Exception. (str "parent neither valid range nor belt node @ leaf count " @leaf-count)))
                                       :no-parent))
                  ;; TODO: check if following TODO is still a TODO?
                  ;; TODO: investigate why when adding the 9th leaf, this new parent hash is the old range nodes hash. Possibly because merge is across ranges?
                  ;; new-parent-hash SHOULD refer to the parent of the range node
                  [new-grandparent-hash child-leg] (if (= :range grandparent-type)
                                                     ;; if parent is range node, this was its left child (since range nodes don't have other range nodes as right children)
                                                     [(clojure.set/union rn (:right (get-parent (get-parent Q-old :range) :range))) :right]
                                                     ;; else, parent is belt - then we must check whether left or right child
                                                     ;; TODO: this check should only be applicable to left-most belt node - all others have a belt node as their left child and a range node as their right
                                                     (if (= :belt grandparent-type)
                                                       (let [left (:left (get-parent (get-parent Q-old :range) :belt))
                                                             right (:right (get-parent (get-parent Q-old :range) :belt))]
                                                         (if (= left (:parent Q-old))
                                                           [(clojure.set/union rn right) :left]
                                                           (if (= right (:parent Q-old))
                                                             [(clojure.set/union left rn) :right])))
                                                       ;; if grandparent neither range nor belt, we just leave blank
                                                       [nil nil]))]
              ;; if Q-old's grandparent is a range node, and Q-old's parent is not the left-child of Q-old's grandparent range, then it's the right-child, hence the range node to the right of Q-old's parent is in another range, so need to hop to it via path: Q-old's right's parent, and then update its left reference (without updating hash, since other range)
              ;; #dbg
              (if (and (:right Q-old)
                       (or (= :no-parent grandparent-type)
                           (and (= :range grandparent-type)
                                (not= (:parent Q-old) (:left (get-parent (get-parent Q-old :range) :range))))))
                #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
                 (swap! range-nodes #(assoc-in % [(:parent (get @node-map (:right Q-old))) :left] rn)))

              ;; if Q-old's grandparent is a belt node, then
              ;; TODO: extend to cover when merge does not occur at rightmost edge of range (does that exist?) - it's just easier like this since already have necessary code above
              ;; #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:belt]))}
              (if (and (= :belt grandparent-type)
                       (or
                        (apply > (map (comp count primitives.core/belt-ranges) [@leaf-count (inc @leaf-count)]))
                        (and (apply = (map (comp count primitives.core/belt-ranges) [@leaf-count (inc @leaf-count)]))
                             ;; NOTE: right thinking, but issue is that mergeable pair has already been removed from stack
                             ;; (distinct-ranges? (get @node-map (:left (get @node-map @lastP))) (get @node-map @lastP))
                             ;; NOTE: instead, check whether @lastP has its own range node :D
                             (= @lastP (:parent (get @node-map @lastP))))))

                (let [old-bn (get-parent (get-parent Q-old :range) :belt)
                      new-bn (belt-node
                              ;; if "hash" of old and new belt node are the same, then we're dealing with a range merge (maybe only for n=6), so old belt node's right child is no longer range leader, so need to use old belt node's
                              (if (= :left child-leg) rn (if (not= (:hash old-bn) new-grandparent-hash) (:left old-bn) (:left (get @belt-nodes (:left old-bn)))))
                              (if (= :right child-leg) rn (:right old-bn))
                              new-grandparent-hash
                              (:parent old-bn))]
                  (swap! belt-nodes #(assoc % new-grandparent-hash new-bn))
                  ;; TODO: this is hacky and simply reasoned from n=6: may not be generally applicable!
                  (if (not= (:hash old-bn) new-grandparent-hash)
                    (swap! belt-nodes #(dissoc % (:hash old-bn)))
                    (let [left-of-old-bn (get @belt-nodes (:left old-bn))]
                      (if (contains? @belt-nodes (:left left-of-old-bn))
                        ;; TODO: check if parent should actually be new-bn in general
                        (swap! belt-nodes #(assoc-in % [(:left left-of-old-bn) :parent] (:hash new-bn)))
                        (if (contains? @range-nodes (:left left-of-old-bn))
                          ;; (if (and (not (contains? #{1 2 5} @leaf-count)) (contains? @range-nodes (:left left-of-old-bn)))
                          #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
                           (swap! range-nodes #(assoc-in % [(:left left-of-old-bn) :parent] (:hash new-bn)))
                          ;; (throw (Exception. (str "old belt node's left child didn't have a left child at " @leaf-count)))
                          ))
                      (swap! belt-nodes #(dissoc % (:hash left-of-old-bn)))))
                  ;; DONE: cover relatives of old-bn & update their references
                  ))
              ;; add new parent range node that couples to old parent range's left
              ;; #dbg
              #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
               (swap! range-nodes #(assoc % rn (range-node (:left (get-parent L :range)) (:hash @Q) rn new-grandparent-hash)))
              ;; update former left's parent's left child to point to rn as a parent
              ;; NOTE: doesn't apply for left-most range node
              ;; #dbg
              #_{:clj-kondo/ignore [:missing-else-branch]}
              (if (and (not distinct-ranges)
                       (:left (get-parent L :range)))
                #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
                 (swap! range-nodes #(assoc-in % [(:left (get-parent L :range)) :parent] rn)))
              ;; remove former left's parent from range nodes
              ;; #dbg
              #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
               (swap! range-nodes #(dissoc % (:parent L)))

              ;; TODO: integrate this neater!
              ;; if range nodes contains old
              (if (and (not distinct-ranges) (contains? @range-nodes (:hash @Q)) (not= 4 @leaf-count))
                ;; #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-merge-replace]))}
                (let [former-range (get @range-nodes (:hash @Q))]
                  #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
                   (swap! range-nodes #(dissoc % (:hash @Q)))
                  (swap! Q #(assoc % :parent rn))
                  ;; if merged peak is not rightmost peak, also update the reference to it from its left neighbour
                  #_{:clj-kondo/ignore [:missing-else-branch]}
                  #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-phantom]))}
                   (if (:right @Q) (swap! range-nodes #(assoc-in % [(:parent (get @node-map (:right @Q))) :left] (:parent @Q))))
                  ;; UNTRUE: if former range's parent is a range node, then former range was a left child
                  ;; (if (contains? @range-nodes (:parent former-range))
                  ;;   (swap! range-nodes #(assoc-in % [(:parent former-range) :left] (:parent @Q))))

                  ;; (swap! range-nodes #(assoc-in % [(:left former-range) :parent] (:parent @Q)))
                  ;; (swap! range-nodes #(assoc-in % [(:left former-range) :parent] (:parent @Q)))
                  ;; (swap! range-nodes #(assoc-in % [(:parent former-range) :right] (:parent @Q)))
                  ))
              ;; TODO: update parent's child reference, and update the parent's other child's parent pointer, and recurse over chain of parents (note: children of parents only need their parent pointer updated - doesn't affect their hash [and hence also not the hash of anything referring to said children])
              )
            (throw (Exception. (str "not handling range nodes with distinct belt nodes above yet @ leaf count " @leaf-count))))))

      ;; add new leaf to node-map
      (swap! node-map #(assoc % (:hash @Q) @Q))
      ;; update :left pointer of Q-old's :right
      #_{:clj-kondo/ignore [:missing-else-branch]}
      (if (:right Q-old)
        (swap! node-map #(assoc-in % [(:right Q-old) :left] (:hash @Q))))
      ;; update :right pointer of L's :left
      #_{:clj-kondo/ignore [:missing-else-branch]}
      (if (:left L)
        (swap! node-map #(assoc-in % [(:left L) :right] (:hash @Q))))
      ;; update new parent-values
      ;; (swap! node-map #(assoc-in % [(:hash L) :parent] (:hash Q)))
      ;; (swap! node-map #(assoc-in % [Q-old-hash :parent] (:hash Q)))

      ;; update new parent-values, change type of children to internal (also removes :right key)
      ;; TODO: simply remove these nodes - they aren't needed beyond debugging and just clutter the interface
      (swap! node-map #(assoc % (:hash L) (internal-node (:left L) (:height L) (:hash L) (:hash @Q))))
      (swap! node-map #(assoc % (:hash Q-old) (internal-node (:left Q-old) (:height Q-old) (:hash Q-old) (:hash @Q))))
      ;; change type of children to internal
      ;; (swap! node-map #(assoc-in % [(:hash L) :type] :internal))
      ;; (swap! node-map #(assoc-in % [Q-old-hash :type] :internal))

      (add-internal (:hash @Q) (inc (inc (* 2 (inc @leaf-count)))))
      ;; issue is that :left of Q can be outdated since may have had subsequent merge
      #_{:clj-kondo/ignore [:missing-else-branch]}
      (if (= (:height @Q) (:height (get @node-map (:left @Q))))
        ;; #dbg
        (let [left's-parent (:parent (get @node-map (:left @Q)))]
          ;; DONE: debug why this broke when adding nil lefty and including last peak from prior range in current range
          (if (or (nil? left's-parent)
                  ;; DONE: This is now a meaningless test since node-map now only contains peaks and internal nodes
                  (contains? @range-nodes left's-parent))
            (add-mergeable-stack @Q)
            (throw (Exception.
                    (str ":left should always be updated whenever we have a merge "
                         "- can't have a non-ephemeral parent! leaf count "
                         @leaf-count))))))

      ;; if we've replaced the old lastP, should reset lastP to point to the new entry
      #_{:clj-kondo/ignore [:missing-else-branch]}
      (if (= (:hash Q-old) @lastP)
        (reset! lastP (:hash @Q)))

      ;; TODO: the following has a smarter integration
      ;; (if (= 4 @leaf-count) (sanity-checks Q-old))

      #_{:clj-kondo/ignore [:unresolved-symbol]}
      (comment #_{:clj-kondo/ignore [:syntax]}
       (if upgrade?)))
    nil))

(defn algo [oneshot-nesting?]
  (let [;; let h be hash of new leaf
        ;; h (str @leaf-count "-hash")
        h #{@leaf-count}
        ;; pointer (get-pointer)
        ;; create object P, set P.hash<-h, set P.height<-0, set P.left<-lastP
        P (peak-node (:hash (get @node-map @lastP)) nil 0 h)
        ;; P (peak-node (:hash (get @node-map @lastP)) nil 0 h)
        ]
    ;; 1. Add step
    ;; store object P in peak map
    (swap! node-map #(assoc % h P))
    ;; (swap! node-map #(assoc % pointer P))
    ;; A[R*n+1]<-h
    (add-internal h (inc (* 2 (inc @leaf-count))))

    ;; 2. Check mergeable
    ;; if lastP.height==0 then M.add(P)
    #_{:clj-kondo/ignore [:missing-else-branch]}
    (if (and @lastP (= (:height (get @node-map @lastP)) 0))
      (add-mergeable-stack (get @node-map h)))

    ;; prelim: set right pointer of lastP to h
    #_{:clj-kondo/ignore [:missing-else-branch]}
    (if @lastP (swap! node-map #(assoc-in % [@lastP :right] h)))
    ;; prelim: create range node for newly appended node if its height difference
    ;; to the last peak is 2, or the last peak can be merged with the mountain to
    ;; its left
    ;; DONE: otherwise, the new node is involved in merge
    ;; #dbg ^{:break/when (not oneshot-nesting?)}
    (new-leaf-range oneshot-nesting? h P)

    ;; 3. reset lastP
    (reset! lastP h)

    ;; 4. merge if mergeable
    (peak-merge oneshot-nesting?)

    (swap! leaf-count inc)

    #_{:clj-kondo/ignore [:missing-else-branch]}
    (if oneshot-nesting? (oneshot-nesting true))
    ;; check (difference (S-n n) (S-n (dec n)))
    ;; recalculate only those members of S-n that are in the difference set from above

    ;; show results
    ;; (clojure.pprint/pprint [@node-map @node-array @mergeable-stack @lastP])
    ;; (clojure.pprint/pprint @node-map)
    ;; (clojure.pprint/pprint @node-map)
    ))

(defn play-algo [n oneshot-nesting?]
  (reset-all)
  (doall (repeatedly n #(algo oneshot-nesting?)))
  ;; (println "-----------------")
  ;; (clojure.pprint/pprint @node-map)
  (state/current-atom-states))

(defn play-algo-retain-sequence
  "play algorithm up to `n`, retaining sequence of intermediate states"
  [n oneshot-nesting?]
  (reset-all)
  (doall (repeatedly n #(do (algo oneshot-nesting?) (state/current-atom-states)))))

;; verify that play-algo & play-algo-retain-sequence match
#_{:clj-kondo/ignore [:missing-else-branch]}
(if run-tests
  (let [n 50
        retain-sequence-oneshot (play-algo-retain-sequence n true)
        retain-sequence-no-oneshot (play-algo-retain-sequence n false)]
    (every? #(and
              (= (play-algo (inc %) true)
                 (nth retain-sequence-oneshot %))
              (= (play-algo (inc %) false)
                 (nth retain-sequence-no-oneshot %)))
            (range n))))

(defn play-algo-manual-end [n]
  (reset-all)
  (doall (repeatedly (dec n) #(algo true)))
  (algo false)
  (state/current-atom-states))

(defn play-algo-optimized
  "plays algorithm without oneshot nesting but for the penultimate step"
  [n]
  (reset-all)
  (doall (repeatedly (dec (dec n)) #(algo false)))
  #_{:clj-kondo/ignore [:missing-else-branch]}
  (if (< 1 n) (algo true))
  #_{:clj-kondo/ignore [:missing-else-branch]}
  (if (< 0 n) (algo false))
  (state/current-atom-states))

(defn play-algo-oneshot-end [n]
  (reset-all)
  (doall (repeatedly (dec n) #(algo false)))
  (algo true)
  (state/current-atom-states))

(defn play-algo-debug-last-step [n]
  (let [global-debugging-state @global-debugging
        debugging-flags-state @debugging-flags]
    (reset! global-debugging false)
    (play-algo (dec n) false)
    (reset! global-debugging true)
    ;; (all-debugging)
    (algo false)
    (reset! global-debugging global-debugging-state)
    (set-debugging-flags debugging-flags-state)))

(defn play-algo-debug-all-steps [n]
  (let [global-debugging-state @global-debugging
        debugging-flags-state @debugging-flags]
    (reset! global-debugging true)
    (all-debugging)
    (play-algo n false)
    (reset! global-debugging global-debugging-state)
    (set-debugging-flags debugging-flags-state)))

;; DONE: must also ensure that parent-child references are symmetric
(defn parent-child-mutual-acknowledgement
  "parent and child mutually reference one another"
  [parent child]
  (and
   (= parent (get-parent child))
   (or (= child (get-child parent :left))
       (= child (get-child parent :right)))))

(defn verify-all-ancestry
  "verifies both the children of `node` and its parent with respect to mutual acknowledgement"
  [node]
  (let [parent (get-parent node)]
    (and
     ;; verify parent
     (if (some? parent)
       (parent-child-mutual-acknowledgement parent node)
       ;; if parent is nil, then must be root belt node
       (= @state/root-belt-node (:hash node)))
     ;; verify left child
     (let [left-child (get-child node :left)]
       (if (some? left-child)
         ;; if the left child is a range node and its parent is a belt node, then the set of children of the left-child and the node must be disjoint
         (or
          (and (= :range (:type node)) (= :range (:type left-child)) (= :belt (:type (get-parent left-child))) (= #{} (clojure.set/intersection (:hash node) (:hash left-child))))
          (parent-child-mutual-acknowledgement node left-child))
         ;; if left child is nil, then must be phantom node
         (= #{} (:hash node))))
     ;; verify right child
     (parent-child-mutual-acknowledgement node (get-child node :right)))))

(play-algo 1337 false)
(let [node (get @state/range-nodes #{1336})
      ;; node (get @state/range-nodes #{})
      ;; node (get @state/range-nodes (into #{} (range 0 1024)))
      ]
  ;; (verify-all-ancestry (get @state/range-nodes #{1336}))
  ;; (truncate-#set-display node)
  ;; (truncate-#set-display (get-child node :left))
  ;; (truncate-#set-display (get-parent (get-child node :left)))
  (verify-all-ancestry node))
;; => true for leaf #{1336} @ (= leaf-count 1337)
;; => true for leaf #{0..1023} @ (= leaf-count 1337)
;; => false for leaf #{} @ (and (= leaf-count 1337) oneshot-nesting?)

(defn verify-range-node-parenting
  "verifies parenting relationships of all range nodes:
  1. parent hash is concatenation of its children, unless left \"child\" is only a phantom reference to another range
  2. verify all ancestry relationships"
  []
  (every? (fn [[k v]] (and
                       (= k (if (and
                                 ;; first, verify that left leaf is not nil
                                 (some? (:left v))
                                 ;; then, verify that left & right "children" are even in the same range
                                 (let [left-child (get-child v :left)]
                                   (if (= :peak (:type left-child))
                                     (distinct-ranges? left-child (get-child v :right))
                                     ;; else
                                     (if (= :range (:type left-child))
                                       (not= v (get-parent left-child))
                                       ;; if parent neither range nor belt, throw exception
                                       (throw (Exception. "unhandled type of left child"))))))
                              ;; if in distinct ranges, then left child does not get included in parent range node's "hash"
                              ;; TODO: stricter condition for distinct ranges of two range nodes
                              (:right v)
                              ;; else, "hash" both
                              (clojure.set/union (or (:left v) #{}) (:right v))))
                       (= #{} (clojure.set/intersection (or (:left v) #{}) (:right v)))
                       (verify-all-ancestry v)))
          (:range-nodes (current-atom-states))))

(defn verify-belt-node-parenting
  "verifies parenting relationships of all belt nodes"
  []
  (every? (fn [[k v]] (and
                       (= k (clojure.set/union (or (:left v) #{}) (:right v)))
                       (= #{} (clojure.set/intersection (or (:left v) #{}) (:right v)))
                       (verify-all-ancestry v)))
          (:belt-nodes (current-atom-states))))

(truncate-#set-display (get (:range-nodes (play-algo-oneshot-end 1337)) #{}))

(defn verify-parenting
  "verifies that all parent hashes are calculated correctly"
  ([cached]
   #_{:clj-kondo/ignore [:missing-else-branch]}
   (if cached
     (state/reset-atoms-from-cached! cached))
   #_{:clj-kondo/ignore [:missing-else-branch]}
   (if (= 0 (mod @state/leaf-count 100)) (println @state/leaf-count))
   (and
    (verify-range-node-parenting)
    (verify-belt-node-parenting)))
  ([]
   (verify-parenting nil)))

#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (let [n 1]
    (reset-all)
    (empty? (filter false?
                    (doall (repeatedly n #(do (algo true) (verify-parenting))))))))
;; => true

#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (let [n 5000]
    (reset-all)
    (last (take-while #(true? (second %))
                      (take n (map-indexed (fn [i v] [i v]) (repeatedly #(do (algo true) (verify-parenting)))))))))
;; => 4999 (i.e. all passed)

#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (let [n 5000]
    (reset-all)
    (last (take-while #(true? (second %))
                      (take n (map-indexed (fn [i v] [i v]) (repeatedly #(do (algo false) (verify-parenting)))))))))
;; => 4999 (i.e. all passed)

;; show that, barring missing belt node impl in incremental algo, get matching
;; result between incremental & oneshot
;; DONE: seems that performance got worse and the following is no longer feasible
;; issue is simply that iterating from scratch every time has a performance impact
;; of (n^2)/2 (i.e. O(n^2)). mitigated by only performing expensive oneshot at the
;; very end (since it's always erased inbetween anyways)
#_{:clj-kondo/ignore [:missing-else-branch]}
(truncate-#set-display
 (if @run-tests
   (let [n 1337]
     [(clojure.set/difference
       (into #{} (vals (:range-nodes (play-algo-oneshot-end n))))
       (into #{} (vals (:range-nodes (play-algo n false)))))
      (clojure.set/difference
       (into #{} (vals (:range-nodes (play-algo n false))))
       (into #{} (vals (:range-nodes (play-algo-oneshot-end n)))))])))
;; => [#{} #{}]

#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (let [n 1337]
    (= (into #{} (play-algo n false))
       (into #{} (play-algo-oneshot-end n))))
  ;; => true
  )

(println "pre-algos:" (new java.util.Date))
(def algo-bound 111)
(def oneshot-only-algos (atom (doall (play-algo-retain-sequence (dec algo-bound) true))))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def oneshot-algos (atom (map #(play-algo-oneshot-end %) (range 1 algo-bound))))
(def manual-algos (atom (doall (map #(play-algo-manual-end %) (range 1 algo-bound)))))
(def manual-only-algos (atom (doall (play-algo-retain-sequence (dec algo-bound) false))))
;; NOTE: extended range of manual-only-algos beyond algo bound
(def manual-only-algos-large (atom (drop 1327 (doall (play-algo-retain-sequence 1337 false)))))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def optimized-manual-algos (atom (map #(play-algo-optimized %) (range 1 algo-bound))))
(println "post-algos:" (new java.util.Date))

(comment
  (with-open [w (clojure.java.io/writer "src/cached.edn")]
    (binding [*out* w]
      (pr @manual-only-algos))))

(def manual-algos-cached
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader "src/cached.edn"))]
    (binding [*read-eval* false]
      (read r))))

(comment
  (with-open [w (clojure.java.io/writer "src/cached-large.edn")]
    (binding [*out* w]
      (pr @manual-only-algos-large))))

(let [fp "src/cached-large.edn"]
  (if (.exists (clojure.java.io/file fp))
    (def manual-algos-cached-large
      (with-open [r (java.io.PushbackReader. (clojure.java.io/reader fp))]
        (binding [*read-eval* false]
          (read r))))
    (throw (Exception. "cached-large.edn has not been created yet - please amend!"))))

;; while upgrading algo, test that new result matches cached
(= manual-algos-cached
   (map #(play-algo % false) (range 1 (inc (count manual-algos-cached)))))
;; => false

;; DONE: update cached nodes to account for phantom belt node - belt-nodes & range-nodes differ
(clojure.test/deftest cache-aligned
  (let [n 1337
        ;; n 110
        cached (nth manual-algos-cached-large (- (dec n) 1327))
        ;; cached (nth manual-algos-cached (dec n))
        fresh (play-algo n false)]
    ;; test that all keys are present
    (clojure.test/are [k] (and (k cached) (k fresh)) :node-map :node-array :mergeable-stack :leaf-count :lastP :belt-nodes :root-belt-node :range-nodes)
    ;; test that all non-map values match
    (clojure.test/are [k] (= (k cached) (k fresh)) :node-array :mergeable-stack :leaf-count :lastP :root-belt-node :joe)
    ;; test that all maps match
    (letfn [(values [m k]
              (into #{} (vals (k m))))
            (diff [k]
              [(clojure.set/difference
                (values cached k)
                (values fresh k))
               (clojure.set/difference
                (values fresh k)
                (values cached k))])]
      (clojure.test/are [k] (= ["#{}" "#{}"] (truncate-#set-display (diff k)))
        :belt-nodes :range-nodes :node-map))))

(clojure.test/run-test cache-aligned)
;; Ran 1 tests containing 17 assertions.
;; 0 failures, 0 errors.

(= manual-algos-cached
   (map #(update % :node-array (comp rest rest))) (map #(play-algo % false) (range 1 (inc (count manual-algos-cached)))))

(map
 (fn [n] (= (nth manual-algos-cached (dec n))
            (play-algo n false))) (range 1 (inc (count manual-algos-cached))))

;; test that everything is exactly the same
(=
 ;; @oneshot-algos
 @oneshot-only-algos
 @manual-algos
 @manual-only-algos
 ;; @optimized-manual-algos
 (map (fn [n] (play-algo-oneshot-end n)) (range 1 algo-bound))
 (map (fn [n] (play-algo n false)) (range 1 algo-bound)))

(doall (map #(play-algo-manual-end %) (range 1 3)))

(comment
  (toggle-debugging)
  (all-debugging)

  (play-algo 1 false)
  (reset! debugging-flags #{:singleton-range})
  (reset! debugging-flags #{:merge})
  (reset! debugging-flags #{:peak-merge})
  (reset! debugging-flags #{:belt})
  (debugging [#{:singleton-range}]))

(letfn [(mapulation [value]
          (identity value))
        ;; (mapulation [value]
        ;;   (dissoc value :parent))
        ;; (mapulation [value]
        ;;   (dissoc (dissoc value :parent) :hash))
        ;; (mapulation [value]
        ;;   (:hash value))
        ]
  (filter
   #(false? (second %))
   (map-indexed (fn [idx n] [(inc idx) (=
                                        (into #{} (map mapulation (vals (:belt-nodes (nth @oneshot-only-algos n)))))
                                        (into #{} (map mapulation (vals (:belt-nodes (nth @manual-algos n))))))])
                (range (count @manual-algos)))))

(letfn [(mapulation [value]
          (identity value))
        ;; (mapulation [value]
        ;;   (dissoc value :parent))
        ]
  (filter
   #(false? (second %))
   (map-indexed (fn [idx n] [(inc idx) (=
                                        (into #{} (map mapulation (vals (:range-nodes (nth @oneshot-only-algos n)))))
                                        (into #{} (map mapulation (vals (:range-nodes (nth @manual-only-algos n))))))])
                (range (min (count @oneshot-only-algos) (count @manual-only-algos))))))

(letfn [(mapulation [value]
          (identity value))
        ;; (mapulation [value]
        ;;   (dissoc value :parent))
        ]
  (filter
   #(false? (second %))
   (map-indexed (fn [idx n] [(inc idx) (=
                                        (into #{} (map mapulation (vals (dissoc (:range-nodes (nth @oneshot-only-algos n)) #{}))))
                                        (into #{} (map mapulation (vals (dissoc (:range-nodes (nth @manual-only-algos n)) #{})))))])
                (range (min (count @oneshot-only-algos) (count @manual-only-algos))))))

(filter #(= "new leaf forms a range alone" ((comp first second) %)) (map-indexed (fn [idx n] [idx (merge-rule n)]) (range 0 60)))

;; test: bj always 1
(every? #(= "1" %)
        (map
         #(let [b-reverse (map str (reverse (primitives.core/binary-repr-of-n (inc %))))
                j (primitives.storage/p-adic-order 2 (inc %))]
            (nth b-reverse j))
         (range 1500)))

(println "pre-big-algos:" (new java.util.Date))
(def algo-1222 (play-algo-oneshot-end 1222))
(def algo-1223 (play-algo-oneshot-end 1223))
(comment
  (def algo-1277 (play-algo-oneshot-end 1277))
  (def algo-1278 (play-algo-oneshot-end 1278))
  (def algo-1279 (play-algo-oneshot-end 1279)))
(println "post-big-algos:" (new java.util.Date))

(def algo-100 (play-algo 100 false))

(prof/serve-files 8080)

;; profile aggregate time spent for full tree
(prof/profile (play-algo 5000 false))

;; profile aggregate time spent for last step of tree
(prof/profile (dotimes [_ 10000]
                (letfn [(reset-from-1222-and-play []
                          (do (state/reset-atoms-from-cached! algo-1222)
                              (algo false)))
                        (reset-from-100-and-play []
                          (do (state/reset-atoms-from-cached! algo-100)
                              (algo false)))]
                  (reset-from-1222-and-play)
                  (reset-from-100-and-play))))

;; test: all peak nodes are connected and can be reached from one another
#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (let [nodes (:node-map algo-1222)
        peaks (filter #(= :peak (:type %)) (vals nodes))
        left-most (filter #(= #{} (:left %)) peaks)
        right-most (filter #(nil? (:right %)) peaks)
        chain-from-left (take-while some? (iterate #(get nodes (:right %)) (first left-most)))
        ;; TODO: might change phantom "peak" to be an actual peak - then don't need the silly condition over here
        chain-from-right (take-while #(and (some? %) (not= #{} (:hash %))) (iterate #(get nodes (:left %)) (first right-most)))]
    {:only-peaks-and-all-peaks
     [;; check that only one node lacks a :left or a :right
      (every? #(= 1 (count %)) [left-most right-most])
      (not= left-most right-most)

      ;; check that only peaks are chained
      (every? #(= :peak (:type %)) chain-from-left)
      (every? #(= :peak (:type %)) chain-from-right)

      (count chain-from-left)
      (count chain-from-right)

      ;; check that every chain contains all peaks
      (every? #(= (count peaks) (count %)) [chain-from-left chain-from-right])]
     :left-most (map :hash left-most)
     :right-most (map :hash right-most)}))

(defn oneshot-nesting-from-cached [cached singleton-ranges?]
  (state/reset-atoms-from-cached! cached)
  ;; (oneshot-nesting)
  (merge (state/current-atom-states) (oneshot-nesting singleton-ranges?)))

(defn oneshot-nesting-from-fresh [n singleton-ranges?]
  (play-algo n true)
  (merge (state/current-atom-states) (oneshot-nesting singleton-ranges?)))

;; test that caching vs fresh has same result
#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (let [fresh-1222 (oneshot-nesting-from-fresh 1222 true)
        cached-1222 (oneshot-nesting-from-cached algo-1222 true)]
    (= fresh-1222 cached-1222)))

(def oneshot-1222 (oneshot-nesting-from-cached algo-1222 true))
(def oneshot-1223 (oneshot-nesting-from-cached algo-1223 true))

(comment
  (keys oneshot-1222)
  (count (:range-nodes oneshot-1222))
  (keys (first (vals (:range-nodes oneshot-1222))))
  (keys oneshot-1222)
  (:belt-nodes oneshot-1222)
  (truncate-#set-display (vals (:belt-nodes oneshot-1222)))
  (truncate-#set-display (:belt-children oneshot-1222))
  (count (:belt-nodes oneshot-1222)))

(filter #(= :peak (:type (get (:node-map algo-1222) (nth (:node-array algo-1222) %))))
        (range (count (:node-array algo-1222))))
(comment (list 1533 1789 2301 2397 2413 2421 2429 2437 2441 2443))

;; NOTE: shifting storage left by 3 since skipping the constant offset from the beginning (always empty)
(map #(- % 3) (sort (primitives.storage/parent-less-nodes 1222)))

(:belt-children (oneshot-nesting-from-fresh 8 true))
(:belt-nodes (oneshot-nesting-from-fresh 8 true))
(def cached-oneshot-9 (oneshot-nesting-from-fresh 9 true))
(identity @range-nodes)
(identity @belt-nodes)

(:range-nodes cached-oneshot-9)
(comment {#{0 1 2 3 4 5 6 7} {:left #{0 1 2 3}, :right #{4 5 6 7}, :hash #{0 1 2 3 4 5 6 7}, :parent #{0 1 2 3 4 5 6 7 8}, :type :range}})
(:belt-nodes cached-oneshot-9)
(comment {#{0 1 2 3 4 5 6 7 8} {:left #{0 1 2 3 4 5 6 7}, :right #{8}, :hash #{0 1 2 3 4 5 6 7 8}, :parent nil, :type :belt}})

(:belt-children (oneshot-nesting-from-fresh 9 true))
(truncate-#set-display (map #(oneshot-nesting-from-fresh % true) (range 1 12)))
(oneshot-nesting-from-fresh 11 true)
(keys (:node-map (play-algo 11 true)))
(:node-array (play-algo 11 true))

(let [n 10
      play (play-algo n true)
      node-array (:node-array play)
      node-map (:node-map play)
      lastP (:lastP play)]
  (first (filter #(= lastP (nth node-array %)) (range (count node-array))))
  ;; (map #(nth @node-array %)
  ;;      (map #(- % 3) (primitives.storage/parent-less-nodes n)))
  )

#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (do (def result-1222-cached (oneshot-nesting-from-fresh 1222 true))
      (def result-1223-cached (oneshot-nesting-from-fresh 1223 true))))
#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (=
   (map #(if (instance? clojure.lang.Atom %) @% %) (vals result-1222-cached))
   (map #(if (instance? clojure.lang.Atom %) @% %) (vals (oneshot-nesting-from-fresh 1222 true))))
  ;; => true
  )

#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (do
    (:type (first (:belt-children result-1222-cached)))
    (get (:node-map result-1222-cached) (:left (first (:belt-children result-1222-cached))))
                                        ; => nil - TODO correct?
    (get (:node-map algo-1222) (:left (first (:belt-children result-1222-cached))))
    (get (:node-map result-1222-cached) (:hash (first (:belt-children result-1222-cached))))

    (nth (map :parent (:node-map result-1222-cached)) 3)
    (map some? (map (fn [val] (get (:range-nodes result-1222-cached) val))
                    (map :hash (filter (fn [entry] (= :range (:type entry)))
                                       (map #(select-keys % [:type :hash]) (:belt-children result-1222-cached))))))
    ;; -> can find all range nodes in collector

    (map some? (map (fn [val] (get (:node-map result-1222-cached) val))
                    (map :hash (filter (fn [entry] (= :range (:type entry)))
                                       ;; (map #(select-keys % [:type :hash]) (:belt-children result-1222) )))))
                                       (:belt-children result-1222-cached)))))
    ;; -> can find first range node in collector

    (map some? (map (fn [val] (:parent (get (:range-nodes result-1222-cached) val)))
                    (map :hash (filter (fn [entry] (= :range (:type entry)))
                                       (map #(select-keys % [:type :hash]) (:belt-children result-1222-cached))))))
    ;; ERGO -> the two last belt children don't have a daddy set, i.e. we're not updating this with final belt node? TODO: Investigate!!!
    ;; update: TODO is this true?

    (count (:range-nodes result-1222-cached))
    (count (:belt-nodes result-1222-cached))
    ;; ERGO -> it adds new range nodes, and the old ones don't attain parents!

    (map some? (map (fn [val] (:parent (get (:range-nodes result-1222-cached) val)))
                    (map :hash (filter (fn [entry] (= :range (:type entry)))
                                       (map #(select-keys % [:type :hash]) (:range-nodes result-1222-cached))))))
    ;; ERGO -> the two last belt children don't have a daddy set, i.e. we're not updating this with final belt node? TODO: Investigate!!!
    ;; update: TODO is this true?

    (map some? (map (fn [val] (get @(:node-map result-1222-cached) val))
                    (map :hash (filter (fn [entry] (= :peak (:type entry))) (map #(select-keys % [:type :hash]) (:belt-children result-1222-cached))))))
    ;; -> peaks have parents set

    (map :type (:belt-children result-1222-cached))

    (nth (:node-array result-1222-cached) 3)

    (map :type (filter #(nil? (:parent %)) (vals (:range-nodes result-1222-cached))))
    ;; (filter (fn [pos] (= (nth @(:node-array result-1222-cached) pos)
    ;;                     ;; (:left (first (filter #(nil? (:parent %)) (vals @(:range-nodes result-1222-cached)))))
    ;;                     (:left (first (filter #(nil? (:parent %)) (vals @(:range-nodes result-1222-cached)))))
    ;;                     )) (range (count @(:node-array result-1222-cached))))

    (first (filter #(nil? (:parent %)) (vals (:range-nodes result-1222-cached))))
    (filter false? (map some? (map :parent (vals (:node-map result-1222-cached)))))

    (count (:range-nodes result-1222-cached))
    (count (:belt-nodes result-1222-cached))
    (get (:range-nodes result-1222-cached)
         (:hash (first (:belt-children result-1222-cached))))

    (map #(get (:node-map algo-1222)
               (nth (:node-array algo-1222) %))
         (map #(- % 3) (primitives.storage/parent-less-nodes 1222)))))

(defn last-algo-match
  "plays algo while the upgrade and old algo still match"
  [upper-bound]
  (inc (first (last (filter #(= true (second %))
                            (map-indexed (fn [index [oneshot full]] [index (= oneshot full)])
                                         (map list
                                              (play-algo-retain-sequence upper-bound false)
                                              (play-algo-retain-sequence upper-bound true))))))))

#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (last-algo-match 1000)
  ;; => 999
  )

(defn first-algo-mismatch
  "plays algo until first mismatch and returns the differences"
  []
  (let [upper-bound 1000
        first-mismatch (inc (last-algo-match upper-bound))]
    (println "-----------------")
    (clojure.pprint/pprint
     {:first-mismatch first-mismatch
      :old (play-algo first-mismatch false)
      :new (play-algo first-mismatch true)})))

(do
  ;; (play-algo 10 false)
  (play-algo 100 false)
  (map (fn [[k v]] [k (:parent v)]) @node-map)
  (keys @node-map))

(keys @node-map)
(get @node-map #{8 9})

(comment
  (algo true)
  (play-algo (last-algo-match 1000) true)
  (first-algo-mismatch))

(comment
  (play-algo 300 false))

;; check that all peaks have correct children
;; TODO: fix
#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
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
          (range (count (primitives.core/S-n @leaf-count)))))

#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests (do
                 (def algo-new-mismatch (play-algo (inc (last-algo-match 1000)) true))
                 (def algo-old-mismatch (play-algo (inc (last-algo-match 1000)) false))

                 (:mergeable-stack algo-new-mismatch)
                 (:mergeable-stack algo-old-mismatch)
                 (:lastP algo-new-mismatch)
                 (:lastP algo-old-mismatch)
                 (clojure.set/difference (into #{} (:node-map algo-new-mismatch))
                                         (into #{} (:node-map algo-old-mismatch)))
                 (clojure.set/difference (into #{} (:node-map algo-old-mismatch))
                                         (into #{} (:node-map algo-new-mismatch)))))

;; test that that tree construction is correct
#_{:clj-kondo/ignore [:missing-else-branch]}
(if @run-tests
  (let [n 1222
        nodes (play-algo n true)
        parent-less (filter #(= nil (:parent (val %))) (:node-map nodes))]
    (every? true?
            [(= (primitives.core/S-n n) (reverse (sort (map (comp :height val) parent-less))))
             (= (primitives.core/S-n n) (reverse (map (comp :height #(get @node-map %))
                                                      (take-while some? (iterate hop-left (:lastP nodes))))))
             (every? nil? (map #(:parent (get @node-map %)) (take-while #(some? (get @node-map %)) (iterate hop-left @lastP))))])))

;; TODO: construct list of edges
(defn graph [n oneshot?]
  (if oneshot?
    (play-algo-oneshot-end n)
    (play-algo n oneshot?))
  (let [peaks (select-keys
               @node-map
               (filter (fn [k] (= :peak (:type (get @node-map k)))) (keys @node-map)))
        edges (apply concat (map (comp truncate-#set-display edges-to-root) (vals peaks)))
        nodes (into #{} (apply concat edges))]
    ;; (truncate-#set-display (edges-to-root (first (vals peaks)) []))
    [;; nodes
     nodes
     ;; edges
     edges
     ;; options
     {:graph {:rankdir :BT
              :label (str "n=" @leaf-count)
              ;; :layout :neato
              }
      :node {:shape :oval}
      :node->id (fn [n] (:id n))
      :node->descriptor (fn [n] (when (map? n) n))}]))

(comment
  (co-path-internal (primitives.storage/leaf-location 65) []))

;; leafs are all correctly stored
;; TODO: set storage-array to match node-array's leaf-count
(= (map first (filter (fn [[_ v]] v) (map-indexed (fn [i v] [i (and (integer? v) (not= 0 v))]) @primitives.storage/storage-array)))
   (map first (filter (fn [[_ v]] v) (map-indexed (fn [i v] [i (and (not= 0 v) (= 1 (count v)))]) @node-array))))

;; nodes are all correctly stored
;; TODO: set storage-array to match node-array's leaf-count
(= (map first (filter (fn [[_ v]] v) (map-indexed (fn [i v] [i (not (integer? v))]) @primitives.storage/storage-array)))
   (map first (filter (fn [[_ v]] v) (map-indexed (fn [i v] [i (and (not= 0 v) (not= 1 (count v)))]) @node-array))))

(primitives.storage/children (primitives.storage/parent-index 121))
(primitives.storage/parent-index (primitives.storage/leaf-location 59))

(comment
  (filter #(= #{60 61} (nth @node-array %)) (range 200))

  (primitives.storage/parent-index 120)

  (get @node-map #{72}))

(keys @range-nodes)
(truncate-#set-display (second (second @range-nodes)))
(truncate-#set-display (second (second @range-nodes)))
(:hash (second (second @range-nodes)))

(comment
  (=
   (second (second @belt-nodes))
   (get-sibling (get-sibling (second (second @belt-nodes))))))

(comment
  ;; (= n 100)
  (get-parent (get-parent (get @node-map #{60})))
  ;; => {:left #{56 57 58 59}, :height 2, :hash #{60 61 62 63}, :parent #{56 57 58 59 60 61 62 63}, :type :internal}
  (last (take 11 (iterate get-parent (get @node-map #{60}))))
  ;; => {:left #{0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95}, :right #{96 97 98 99}, :hash #{0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99}, :parent nil, :type :belt}
  (last (take 12 (iterate get-parent (get @node-map #{60}))))
  ;; => nil
  )

(defn get-nodes
  "returns all nodes that have given hash, optionally filtered by `type-contenders`"
  ([hash type-contenders]
   (filter some? (map #(let [entry (get @(get storage-maps %) hash)]
                         (if (= (:type entry) %)
                           entry
                           nil))
                      (filter #(contains? type-contenders %)
                              (keys storage-maps)))))
  ([hash]
   (get-nodes hash (into #{} (keys storage-maps)))))

(comment
  (map :type (get-nodes #{}))
  (types #{}))

(println "end:" (new java.util.Date))

(toggle-tests)
