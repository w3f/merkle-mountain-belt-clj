(ns linked-peaks
  (:require [core]))

;; generic tooling
(def global-debugging (atom false))
(defn toggle-debugging [] (swap! global-debugging #(not %)))
(comment (toggle-debugging))
(defn all-debugging []
  (reset! debugging-flags #{:singleton-range :merge :belt-merge :range-merge-replace :peak-merge}))
(def debugging-flags (atom #{:singleton-range :merge :belt-merge :range-merge-replace}))
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
   data
   )
  )

(comment
  "example usage"
  (truncate-#set-display (:belt-nodes (oneshot-nesting-from-fresh 8 true)))
  (truncate-#set-display (:range-nodes (oneshot-nesting-from-fresh 8 true)))
  )

(defn display-type-filtered [data type]
  (truncate-#set-display
   (filter #(= type (:type %)) data)))

(comment
  "example usage"
  (display-type-filtered (vals (:node-map (play-algo 1222 true))) :peak)
  (display-type-filtered (vals (:node-map (play-algo 20 true))) :internal)
)

(defn merge-rule [n]
  (let [
        b (storage/binary-repr-of-n (inc n))
        j (storage/p-adic-order 2 (inc n))
        m1 (if (< 1 j) "new leaf joins last range"
               (get {0 "new leaf participates in merge"
                     1 "new leaf forms a range alone"} j))
        [bj+2 bj+1 bj] (if (< (+ j 2) (count b))
                         (map #(Integer/parseInt (str %)) (map #(nth (reverse b) %) [(+ j 2) (+ j 1) j]))
                         )
        m2 (if (= [bj+1 bj] [0 1])
             "M.M. not alone in range"
             (if (= [bj+2 bj+1 bj] [0 1 1])
               "M.M. alone in range"
               (if (= [bj+2 bj+1 bj] [1 1 1])
                 "M.M. joins prev range"
                 )))
        ]
    [m1 m2]
    ))

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

(def lastP (atom #{}))

;; (def R-count (atom 0))

(def mergeable-stack (atom []))

(def leaf-count (atom 0))

(def node-map (atom {}))
(def node-array (atom []))
(def belt-nodes (atom {}))
;; NOTE: this is a hack to actually point to the dummy range node
;; TODO: potentially find more elegant solution since this hack may introduce bugs
(def root-belt-node (atom #{}))
(def range-nodes (atom {}))
(def belt-children (atom {}))

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
   ;; NOTE: dummy node is not a valid peak
   (reset! node-map {#{} {:height ##Inf :hash #{} :parent #{}}})
   (reset! node-array [])
   (reset! mergeable-stack [])
   (reset! leaf-count 0)
   (reset! lastP #{})
   (reset! belt-nodes {})
   (reset! root-belt-node #{})
   ;; TODO: remove range node hack here
   (reset! range-nodes {#{} {:left nil, :right #{}, :hash #{}, :parent #{0}, :type :range}})
   ))

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

(defn sanity-checks [Q-old]
  (if (= (:hash Q-old) (hop-left @lastP))
    (throw (Exception. ":left of lastP is outdated")))
  (if (= (:hash Q-old) (:left (get @node-map (:left (get @node-map @lastP)))))
    (throw (Exception. ":left of lastP's left is outdated")))
  (let [
        left-most-sibling-peak (last (take-while #(and (some? %) (not (contains? #{:internal :peak} (:type (get @node-map (hop-parent %)))))) (iterate hop-left @lastP)))
        correct-sibling-of-left-most (take-while #(and (some? %) (contains? #{:internal :peak} (:type (get @node-map %)))) (iterate hop-parent (hop-left left-most-sibling-peak)))
        ]
    (if (and (some? left-most-sibling-peak) (< 1 (count correct-sibling-of-left-most)))
      (throw (Exception. "should never get :left dissociated"))
      ;; #dbg
      ;; (swap! node-map #(assoc-in % [left-most-sibling-peak :left] (last correct-sibling-of-left-most)))
      )))

(comment (:range-nodes (play-algo 5 true))
         (oneshot-nesting-from-fresh 8 false)
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

(defn oneshot-nesting
  "performs a oneshot nesting of ephemeral range and belt nodes. takes flag `singleton-ranges?` to specify whether singleton peaks should also have a range node above them"
  [singleton-ranges?]
  (let [
        ;; {:keys [node-map node-array]} (select-keys (play-algo @leaf-count upgrade?) [:node-map :node-array])
        ;; node-map (atom (:node-map algo-1222))
        ;; node-array (atom (:node-array algo-1222))
        ;; range-nodes (atom {})
        ;; belt-nodes (atom {})
        original-sorted-peaks (map #(get @node-map (nth @node-array (- (first %) 3))) (storage/parent-less-nodes-sorted-height (storage/parent-less-nodes @leaf-count)))
        ;; prepend nil as a peak to facilitate a linked list of peaks. TODO: abuse this as a pointer for the left-most peak ^^
        sorted-peaks (atom (if singleton-ranges? (cons (peak-node nil (:hash (first original-sorted-peaks)) ##Inf #{}) original-sorted-peaks) original-sorted-peaks))
        ;; sorted-peaks (atom (if singleton-ranges? (cons (peak-node #{} (:hash (first original-sorted-peaks)) ##Inf #{}) original-sorted-peaks) original-sorted-peaks))
        ]
    (reset! range-nodes {})
    (reset! belt-nodes {})
    (letfn [
            ;; takes type of child to find its storage map, and then updates its parent
            (update-parent [parent child]
              (swap! (get storage-maps (:type child)) (fn [storage-map] (assoc-in storage-map [(:hash child) :parent] (:hash parent)))))
            ]
      ;; #dbg
      (let [
            belt-children (doall (map (fn [belt-range-count]
                                        (reduce (fn [left-child right-child]
                                                  (let [left-most (:intruder left-child)
                                                        rn (range-node (:hash left-child) (:hash right-child)
                                                                       ;; NOTE: ugly hack to use (:hash right-child) first since
                                                                       ;; (= nil (clojure.set/union nil #{}))
                                                                       ;; but
                                                                       ;; (= #{} (clojure.set/union #{} nil))
                                                                       (clojure.set/union (:hash right-child) (if-not (and singleton-ranges? left-most) (:hash left-child)))
                                                                       ;; (clojure.set/union (if-not (and singleton-ranges? left-most) (:hash left-child)) (:hash right-child))
                                                                       nil)]
                                                    (doall (map
                                                            (partial update-parent rn)
                                                            (if (and singleton-ranges? left-most) [right-child] [left-child right-child])))
                                                    (swap! range-nodes (fn [range-nodes] (assoc range-nodes (:hash rn) rn)))
                                                    rn
                                                    ))
                                                ;; returns all peaks that are in the given range.
                                                ;; for every iteration, include the last node from the prior range, to make a linked list of all range nodes.
                                                (update (into [] (if singleton-ranges?
                                                                   (let [[dropped remainder] (split-at (inc belt-range-count) @sorted-peaks)
                                                                         new-leader (apply clojure.set/union (map :hash (rest dropped)))]
                                                                     ;; NOTE: since sorted-peaks is never read again after last step, the (if (empty? remainder) ..) check is in fact superfluous, but putting it in nonetheless, in case this features as a bug later
                                                                     (reset! sorted-peaks (if (empty? remainder) remainder (cons {:hash new-leader} remainder)))
                                                                     (if (< 1 (count dropped))
                                                                       dropped
                                                                       (conj dropped {}))
                                                                     )
                                                                   (take belt-range-count
                                                                         (first (swap-vals! sorted-peaks (fn [current] (drop belt-range-count current)))))
                                                                   ))
                                                        ;; DONE: first value shouldn't be last peak, but the actual range node's hash, i.e. the concatenation of hashes of the entire range
                                                        ;; tags the first node as NOT being in the same range
                                                        0 #(if singleton-ranges? (assoc % :intruder true) %)
                                                        )))
                                      ;; returns number of nodes in each range
                                      (map count (cons [] (core/belt-ranges @leaf-count)))))
            ;; belt-children ()
            root-bn (doall
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
        (reset! root-belt-node (:hash root-bn))
        {:belt-children belt-children
         :range-nodes @range-nodes
         :belt-nodes @belt-nodes
         :root-belt-node @root-belt-node
         ;; :node-map node-map
         ;; :node-array node-array
         }))
    ))

(defn distinct-ranges? [M M']
  (or (= 2 (- (:height M) (:height M')))
      (contains? (into #{} @mergeable-stack) (:hash M))
      ;; TODO: might be able to remove the following if/once have unified rules independent of singleton-ness of new leaf
      (nil? (:hash M))
      (= #{} (:hash M))
    ))

(defn new-leaf-range [oneshot-nesting? h P]
  #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:singleton-range]))}
  ;; DONE: if distinct ranges, we're also adding a new belt node for the new leaf
  (if (distinct-ranges? (get @node-map @lastP) P)
    (do
      ;; create new root belt node with new leaf's parent range node as right child and former root node as left child
      (let [new-belt-root (clojure.set/union h @root-belt-node)]
        (swap! belt-nodes #(assoc % new-belt-root (belt-node @root-belt-node h new-belt-root nil)))
        ;; TODO: skipping #{} because?
        (if (not= #{} @root-belt-node) (swap! belt-nodes #(assoc-in % [@root-belt-node :parent] new-belt-root)))
        (reset! root-belt-node new-belt-root)
        ;; TODO: don't step into range node map to get hash - it's already in the peak's parent reference
        (swap! range-nodes #(assoc % h (range-node (:hash (get @range-nodes (:parent (get @node-map @lastP)))) h h new-belt-root)))
        )

      (swap! node-map #(assoc-in % [h :parent] h))
      ;; conditional here is a temporary hack since I don't wanna bother with implementing correct logic yet
      (if (>= @leaf-count 8)
        (let [last-range-node-hash (:parent (get @node-map @lastP))
              last-belt-node-hash (:parent (get @range-nodes last-range-node-hash))
              new-belt-hash (clojure.set/union (or last-belt-node-hash last-range-node-hash) h)]
          (swap! belt-nodes #(assoc % new-belt-hash (belt-node (or last-belt-node-hash last-range-node-hash) h new-belt-hash nil)))))
      )
    ;; else new leaf joins last range, i.e. get new range node above new leaf
    ;; TODO: update parent belt node hash, likewise for its left sibling
    (let [last-range (get @range-nodes (:parent (get @node-map @lastP)))
          old-belt-parent (get @belt-nodes (:parent last-range))
          hash-new-range (clojure.set/union (:hash last-range) h)
          new-belt-parent (clojure.set/union hash-new-range (:left old-belt-parent))
          new-range (range-node (:hash last-range) h hash-new-range new-belt-parent)
          ]
      (do
        (swap! range-nodes #(assoc % (:hash new-range) new-range))
        (swap! node-map #(assoc-in % [h :parent] (:hash new-range)))
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
            (swap! range-nodes #(assoc-in % [(:left old-belt-parent) :parent] new-belt-parent))
            (throw (Exception. (str "old belt node's left child was invalid at leaf count " @leaf-count)))
            ))
        (swap! belt-nodes #(dissoc % (:hash old-belt-parent)))
        (reset! root-belt-node new-belt-parent)
        ;; TODO: update siblings around update

        ))
    ))

(defn types
  "returns all types that a given hash has an entry for"
  [hash]
  (into #{} (map :type (filter some? (map #(get @% hash) [node-map range-nodes belt-nodes])))))

(defn get-node
  "returns the node with a given hash, as long as it exists for the provided type"
  [hash type]
  (get @(get storage-maps type) hash))

(def type-rank
  (zipmap
   [:internal :peak :range :belt]
   (range)))

(defn higher-type-ranks [type]
  (filter #(< (get type-rank type) (get type-rank %)) (keys type-rank)))

(comment
  (higher-type-ranks :peak)
  ; => (:range :belt)
  )

(defn parent-type [type]
  (get (clojure.set/map-invert type-rank) (inc (get type-rank type))))

(defn get-nodes
  "returns all nodes that have given hash, optionally filtered by `type-contenders`"
  ([hash type-contenders]
   (filter some? (map #(let [entry (get @(get storage-maps %) hash)]
                         (if (= (:type entry) %)
                           entry))
                      (filter #(contains? type-contenders %)
                              (keys storage-maps))
                      )))
  ([hash]
   (get-nodes hash (into #{} (keys storage-maps)))))

(comment
  (map :type (get-nodes #{}))
  (types #{}))

(defn peak-merge [oneshot-nesting?]
  ;; #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:peak-merge]))}
  (if (not (zero? (count @mergeable-stack)))
        (do
          (let [Q (atom (pop-mergeable-stack))
                Q-old @Q
                ;; Q-old-hash (:hash Q-old)
                L (get @node-map (:left @Q))
                ]

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
                (let [
                      ;; check where parent lives: should only exist in one of the maps
                      parent-contenders (get-nodes (:parent Q-old))
                      ]
                  ;; refactor here by splitting head of contenders from tail in let binding
                  (if (= 1 (count parent-contenders))
                    (if (not (contains? #{:internal :peak} (:type (first parent-contenders))))
                      (((:type (first parent-contenders)) {
                                                           :range (fn [] (do
                                                                          (swap! Q #(assoc % :parent (:parent (first parent-contenders))))
                                                                          (swap! range-nodes #(dissoc % (:hash (first parent-contenders))))
                                                                          ;; (throw (Exception. "unimplemented"))
                                                                          ))
                                                           :belt (fn [] (throw (Exception. "peak can't have belt node as parent!")))
                                                           }))
                      (throw (Exception. "parent is an illegal: internal or peak"))
                      )
                    (throw (Exception. "multiple parent contenders - no bueno!"))
                    )
                  )
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
                         (= (:parent Q-old) (:parent (get @range-nodes (:parent L))))
                         (= (:parent L) (:left (get @range-nodes (:parent Q-old))))
                         )
                  ;; #dbg

                  ;; #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:merge]))}
                  (let [
                        parent-L (get @range-nodes (:parent L))
                        ;; this is the range node that will replace their former parent range nodes
                        ;; if the range node above former left is not leftmost node, include its left in the hash (otherwise, its left is in another range)
                        ;; DONE: update the nodes referred to to be left: left, right: newly merged peak
                        ;; TODO: should kill old parent range node that's no longer applicable
                        distinct-ranges (distinct-ranges? (get @node-map (:left @Q)) @Q)
                        rn (clojure.set/union (if (not distinct-ranges)
                                                (:left parent-L))
                                              (:hash @Q))
                        ;; DONE (fixed above): the following currently only *preserves* range splits - should check whether the two range nodes should now be in the same range
                        ;; rn (clojure.set/union (if (not= (:hash parent-L) (:right parent-L)) (:left parent-L)) (:hash @Q))
                        ;; Q-old is a peak node, so its immediate parent is certainly a range node. The only unknown is the type of the parent's parent
                        grandparent-type (if (contains? @range-nodes (:parent (get @range-nodes (:parent Q-old))))
                                      :range
                                      (if (contains? @belt-nodes (:parent (get @range-nodes (:parent Q-old))))
                                        :belt
                                        ;; (throw (Exception. (str "parent neither valid range nor belt node @ leaf count " @leaf-count)))
                                        :no-parent
                                      ))
                        ;; TODO: check if following TODO is still a TODO?
                        ;; TODO: investigate why when adding the 9th leaf, this new parent hash is the old range nodes hash. Possibly because merge is across ranges?
                        ;; new-parent-hash SHOULD refer to the parent of the range node
                        [new-grandparent-hash child-leg] (if (= :range grandparent-type)
                                                           ;; if parent is range node, this was its left child (since range nodes don't have other range nodes as right children)
                                                           [(clojure.set/union rn (:right (get @range-nodes (:parent (get @range-nodes (:parent Q-old)))))) :right]
                                                           ;; else, parent is belt - then we must check whether left or right child
                                                           ;; TODO: this check should only be applicable to left-most belt node - all others have a belt node as their left child and a range node as their right
                                                           (if (= :belt grandparent-type)
                                                             (let [left (:left (get @belt-nodes (:parent (get @range-nodes (:parent Q-old)))))
                                                                   right (:right (get @belt-nodes (:parent (get @range-nodes (:parent Q-old)))))]
                                                               (if (= left (:parent Q-old))
                                                                 [(clojure.set/union rn right) :left]
                                                                 (if (= right (:parent Q-old))
                                                                   [(clojure.set/union left rn) :right])
                                                                 ))
                                                             ;; if grandparent neither range nor belt, we just leave blank
                                                             [nil nil])
                                                      )]
                    ;; if Q-old's grandparent is a range node, and Q-old's parent is not the left-child of Q-old's grandparent range, then it's the right-child, hence the range node to the right of Q-old's parent is in another range, so need to hop to it via path: Q-old's right's parent, and then update its left reference (without updating hash, since other range)
                    ;; #dbg
                    (if (and (:right Q-old)
                         (or (= :no-parent grandparent-type)
                             (and (= :range grandparent-type)
                                  (not= (:parent Q-old) (:left (get @range-nodes (:parent (get @range-nodes (:parent Q-old)))))))))
                      (swap! range-nodes #(assoc-in % [(:parent (get @node-map (:right Q-old))) :left] rn))
                      )

                    ;; if Q-old's grandparent is a belt node, then
                    ;; TODO: extend to cover when merge does not occur at rightmost edge of range (does that exist?) - it's just easier like this since already have necessary code above
                    #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:belt]))}
                    (if (and (= :belt grandparent-type)
                             (or
                              (apply > (map (comp count core/belt-ranges) [@leaf-count (inc @leaf-count)]))
                              (and (apply = (map (comp count core/belt-ranges) [@leaf-count (inc @leaf-count)]))
                                   ;; NOTE: right thinking, but issue is that mergeable pair has already been removed from stack
                                   ;; (distinct-ranges? (get @node-map (:left (get @node-map @lastP))) (get @node-map @lastP))
                                   ;; NOTE: instead, check whether @lastP has its own range node :D
                                   (= @lastP (:parent (get @node-map @lastP)))
                                   )
                              )
                             )
                      (let [
                            old-bn (get @belt-nodes (:parent (get @range-nodes (:parent Q-old))))
                            new-bn (belt-node
                                    ;; if "hash" of old and new belt node are the same, then we're dealing with a range merge (maybe only for n=6), so old belt node's right child is no longer range leader, so need to use old belt node's 
                                    (if (= :left child-leg) rn (if (not= (:hash old-bn) new-grandparent-hash) (:left old-bn) (:left (get @belt-nodes (:left old-bn)))))
                                    (if (= :right child-leg) rn (:right old-bn))
                                    new-grandparent-hash
                                    (:parent old-bn))
                            ]
                        (swap! belt-nodes #(assoc % new-grandparent-hash new-bn))
                        ;; TODO: this is hacky and simply reasoned from n=6: may not be generally applicable!
                        (if (not= (:hash old-bn) new-grandparent-hash)
                          (swap! belt-nodes #(dissoc % (:hash old-bn)))
                          (do
                            (let [left-of-old-bn (get @belt-nodes (:left old-bn))]
                              (if (contains? @belt-nodes (:left left-of-old-bn))
                                ;; TODO: check if parent should actually be new-bn in general
                                (swap! belt-nodes #(assoc-in % [(:left left-of-old-bn) :parent] (:hash new-bn)))
                                (if (contains? @range-nodes (:left left-of-old-bn))
                                  (swap! range-nodes #(assoc-in % [(:left left-of-old-bn) :parent] (:hash new-bn)))
                                  (throw (Exception. (str "old belt node's left child didn't have a left child at " @leaf-count)))
                                  ))
                              (swap! belt-nodes #(dissoc % (:hash left-of-old-bn)))))
                          )
                        ;; TODO: cover relatives of old-bn & update their references
                        ))
                    ;; add new parent range node that couples to old parent range's left
                    ;; #dbg
                    (swap! range-nodes #(assoc % rn (range-node (:left (get @range-nodes (:parent L))) (:hash @Q) rn new-grandparent-hash)))
                    ;; update former left's parent's left child to point to rn as a parent
                    ;; NOTE: doesn't apply for left-most range node
                    ;; #dbg
                    (if (and (not distinct-ranges)
                             (:left (get @range-nodes (:parent L))))
                      (swap! range-nodes #(assoc-in % [(:left (get @range-nodes (:parent L))) :parent] rn)))
                    ;; remove former left's parent from range nodes
                    ;; #dbg
                    (swap! range-nodes #(dissoc % (:parent L)))

                    ;; TODO: integrate this neater!
                    ;; if range nodes contains old
                    (if (and (not distinct-ranges) (contains? @range-nodes (:hash @Q)) (not= 4 @leaf-count))
                      #dbg ^{:break/when (and (not oneshot-nesting?) (debugging [:range-merge-replace]))}
                      (let [former-range (get @range-nodes (:hash @Q))]
                        (swap! range-nodes #(dissoc % (:hash @Q)))
                        (swap! Q #(assoc % :parent rn))
                        ;; if merged peak is not rightmost peak, also update the reference to it from its left neighbour
                        (if (:right @Q) (swap! range-nodes #(assoc-in % [(:parent (get @node-map (:right @Q))) :left] (:parent @Q))))
                        ;; UNTRUE: if former range's parent is a range node, then former range was a left child
                        ;; (if (contains? @range-nodes (:parent former-range))
                        ;;   (swap! range-nodes #(assoc-in % [(:parent former-range) :left] (:parent @Q))))

                        ;; (swap! range-nodes #(assoc-in % [(:left former-range) :parent] (:parent @Q)))
                        ;; (swap! range-nodes #(assoc-in % [(:left former-range) :parent] (:parent @Q)))
                        ;; (swap! range-nodes #(assoc-in % [(:parent former-range) :right] (:parent @Q)))
                        )
                      )

                    ;; TODO: update parent's child reference, and update the parent's other child's parent pointer, and recurse over chain of parents (note: children of parents only need their parent pointer updated - doesn't affect their hash [and hence also not the hash of anything referring to said children])
                    )
                  (throw (Exception. (str "not handling range nodes with distinct belt nodes above yet @ leaf count " @leaf-count)))
                )
                ))

            ;; add new leaf to node-map
            (swap! node-map #(assoc % (:hash @Q) @Q))
            ;; update :left pointer of Q-old's :right
            (if (:right Q-old)
              (swap! node-map #(assoc-in % [(:right Q-old) :left] (:hash @Q))))
            ;; update :right pointer of L's :left
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

            (add-internal (:hash @Q) (inc (* 2 @leaf-count)))
            ;; issue is that :left of Q can be outdated since may have had subsequent merge
            (if (= (:height @Q) (:height (get @node-map (:left @Q))))
              ;; #dbg
              (let [left's-parent (:parent (get @node-map (:left @Q)))]
                ;; DONE: debug why this broke when adding nil lefty and including last peak from prior range in current range
                (if (or (nil? left's-parent)
                        ;; DONE: This is now a meaningless test since node-map now only contains peaks and internal nodes
                        (contains? @range-nodes left's-parent)
)
                 (add-mergeable-stack @Q)
                 (throw (Exception. (str ":left should always be updated whenever we have a merge - can't have a non-ephemeral parent! leaf count " @leaf-count)))
                 ))
              )

            ;; if we've replaced the old lastP, should reset lastP to point to the new entry
            (if (= (:hash Q-old) @lastP)
              (reset! lastP (:hash @Q)))

            ;; TODO: the following has a smarter integration
            ;; (if (= 4 @leaf-count) (sanity-checks Q-old))

            (comment (if upgrade?))

            )
          )
        ))

(defn algo [oneshot-nesting?]
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
      (if @lastP (swap! node-map #(assoc-in % [@lastP :right] h)))
      ;; prelim: create range node for newly appended node if its height difference to the last peak is 2, or the last peak can be merged with the mountain to its left
      ;; DONE: otherwise, the new node is involved in merge
      ;; #dbg ^{:break/when (not oneshot-nesting?)}
      (new-leaf-range oneshot-nesting? h P)

      ;; 3. reset lastP
      (reset! lastP h)

      ;; 4. merge if mergeable
      (peak-merge oneshot-nesting?)

      (swap! leaf-count inc)

      (if oneshot-nesting? (oneshot-nesting true))
      ;; check (difference (S-n n) (S-n (dec n)))
      ;; recalculate only those members of S-n that are in the difference set from above

      ;; show results
      ;; (clojure.pprint/pprint [@node-map @node-array @mergeable-stack @lastP])
      ;; (clojure.pprint/pprint @node-map)
      ;; (clojure.pprint/pprint @node-map)
      ))
  )

(defn reset-atoms-from-cached [cached]
  (reset! node-map (:node-map cached))
  (reset! node-array (:node-array cached))
  (reset! mergeable-stack (:mergeable-stack cached))
  (reset! lastP (:lastP cached))
  (reset! leaf-count (:leaf-count cached))
  (reset! belt-nodes (:belt-nodes cached))
  (reset! range-nodes (:range-nodes cached))
  )

(defn current-atom-states []
  {
   :node-map @node-map
   :node-array @node-array
   :mergeable-stack @mergeable-stack
   :leaf-count @leaf-count
   :lastP @lastP
   :belt-nodes @belt-nodes
   :root-belt-node @root-belt-node
   :range-nodes @range-nodes
   })


(defn play-algo [n oneshot-nesting?]
  (do (reset-all)
      (doall (repeatedly n #(algo oneshot-nesting?)))
      ;; (println "-----------------")
      ;; (clojure.pprint/pprint @node-map)
      (current-atom-states)
      ))

(defn play-algo-manual-end [n]
  (do (reset-all)
      (doall (repeatedly (dec n) #(algo true)))
      (algo false)
      (current-atom-states)
      ))

(defn play-algo-optimized [n]
  (do (reset-all)
      (doall (repeatedly (dec (dec n)) #(algo false)))
      (if (< 1 n) (algo true))
      (if (< 0 n) (algo false))
      (current-atom-states)
      ))

(defn play-algo-oneshot-end [n]
  (do (reset-all)
      (doall (repeatedly (dec n) #(algo false)))
      (algo true)
      (current-atom-states)
      ))

;; show that, barring missing belt node impl in incremental algo, get matching result between incremental & oneshot
;; DONE: seems that performance got worse and the following is no longer feasible
;; issue is simply that iterating from scratch every time has a performance impact of (n^2)/2 (i.e. O(n^2)). mitigated by only performing expensive oneshot at the very end (since it's always erased inbetween anyways)
(let [n 3000]
  (= (into #{} (map #(dissoc % :parent) (vals (:range-nodes (play-algo n false)))))
     (into #{} (map #(dissoc % :parent) (vals (:range-nodes (play-algo-oneshot-end n)))))))

(def algo-bound 101)
(def oneshot-only-algos (atom (doall (map #(play-algo % true) (range 1 algo-bound)))))
(def oneshot-algos (atom (doall (map #(play-algo-oneshot-end %) (range 1 algo-bound)))))
(def manual-algos (atom (doall (map #(play-algo-manual-end %) (range 1 algo-bound)))))
(def manual-only-algos (atom (doall (map #(play-algo % false) (range 1 algo-bound)))))
(def optimized-manual-algos (atom (doall (map #(play-algo-optimized %) (range 1 algo-bound)))))

;; while upgrading algo, test that new result matches cached
(= @manual-algos
   (map #(play-algo-oneshot-end %) (range 1 algo-bound)))

;; test that everything is exactly the same
(= @oneshot-algos
   @oneshot-only-algos
   @manual-algos
   @manual-only-algos
   @optimized-manual-algos)


(toggle-debugging)
(all-debugging)
(letfn [
        ;; (mapulation [value]
        ;;   (identity value))
        (mapulation [value]
          (dissoc value :parent))
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

(letfn [
        (mapulation [value]
          (identity value))
        ;; (mapulation [value]
        ;;   (dissoc value :parent))
        ]
  (filter
   #(false? (second %))
   (map-indexed (fn [idx n] [(inc idx) (=
                                       (into #{} (map mapulation (vals (:range-nodes (nth @oneshot-only-algos n)))))
                                       (into #{} (map mapulation (vals (:range-nodes (nth @optimized-manual-algos n))))))])
                (range (min (count @oneshot-only-algos) (count @optimized-manual-algos))))))

(letfn [
        (mapulation [value]
          (identity value))
        ;; (mapulation [value]
        ;;   (dissoc value :parent))
        ]
  (filter
   #(false? (second %))
   (map-indexed (fn [idx n] [(inc idx) (=
                                       (into #{} (map mapulation (vals (dissoc (:range-nodes (nth @oneshot-only-algos n)) #{}))))
                                       (into #{} (map mapulation (vals (dissoc (:range-nodes (nth @manual-only-algos n)) #{})))))])
                (range (min (count @oneshot-only-algos) (count @optimized-manual-algos))))))

(filter #(= "new leaf forms a range alone" ((comp first second) %)) (map-indexed (fn [idx n] [idx (merge-rule n)]) (range 0 60)))

;; test: bj always 1
(every? #(= "1" %)
        (map
         #(let [b-reverse (map str (reverse (storage/binary-repr-of-n (inc %))))
                j (storage/p-adic-order 2 (inc %))]
            (nth b-reverse j))
         (range 1500)))

(def algo-1222 (play-algo-oneshot-end 1222))
(def algo-1223 (play-algo 1223 true))
(def algo-1277 (play-algo 1277 true))
(def algo-1278 (play-algo 1278 true))
(def algo-1279 (play-algo 1279 true))

;; test: all peak nodes are connected and can be reached from one another
(let [nodes (:node-map algo-1222)
      peaks (filter #(not= :internal (:type %)) (vals nodes))
      left-most (filter #(= #{} (:left %)) peaks)
      right-most (filter #(nil? (:right %)) peaks)
      chain-from-left (take-while some? (iterate #(get nodes (:right %)) (first left-most)))
      chain-from-right (take-while some? (iterate #(get nodes (:left %)) (first right-most)))
      ]
  {:only-peaks-and-all-peaks
   (and
    ;; check that only one node lacks a :left or a :right
    (every? #(= 1 (count %)) [left-most right-most])
    (not= left-most right-most)

    ;; check that only peaks are chained
    (every? #(= :peak (:type %)) chain-from-left)
    (every? #(= :peak (:type %)) chain-from-right)

    ;; check that every chain contains all peaks
    (every? #(= (count peaks) (count %)) [chain-from-left chain-from-right])
    )
   :left-most (map :hash left-most)
   :right-most (map :hash right-most)}
  )

(defn oneshot-nesting-from-cached [cached singleton-ranges?]
  (do (reset-atoms-from-cached cached)
      ;; (oneshot-nesting)
      (merge (current-atom-states) (oneshot-nesting singleton-ranges?))))

(defn oneshot-nesting-from-fresh [n singleton-ranges?]
  (do (play-algo n true)
      (merge (current-atom-states) (oneshot-nesting singleton-ranges?))))

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
(=
 (map #(if (instance? clojure.lang.Atom %) @% %) (vals result-1222-cached))
 (map #(if (instance? clojure.lang.Atom %) @% %) (vals (oneshot-nesting-from-fresh 1222))))
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
(count @(:belt-nodes result-1222))
;; ERGO -> it adds new range nodes, and the old ones don't attain parents!

(map some? (map (fn [val] (:parent (get @(:range-nodes result-1222) val)))
                (map :hash (filter (fn [entry] (= :range (:type entry)))
                                   (map #(select-keys % [:type :hash]) @(:range-nodes result-1222))))))
;; ERGO -> the two last belt children don't have a daddy set, i.e. we're not updating this with final belt node? TODO: Investigate!!!

(map some? (map (fn [val] (get @(:node-map result-1222) val))
      (map :hash (filter (fn [entry] (= :peak (:type entry))) (map #(select-keys % [:type :hash]) (:belt-children result-1222) )))))
;; -> peaks have parents set


(count @(:range-nodes result-1222))
(count @(:belt-nodes result-1222))
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

(last-algo-match)

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
