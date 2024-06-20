(ns core
  (:require
   clojure.set
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as sgen] ;; [clojure.spec.test.alpha :as stest]
   [primitives.core :refer [belt-ranges children has-children? S-n]]
   [primitives.storage]
   [rhizome.viz :as viz]
   [tangle.core :as tangle]))

(println "start:" (new java.util.Date))

(defonce index (atom -1))
(defonce leaf-index (atom -1))

(def labeling (atom :join))
(reset! labeling :set)

(s/def ::hash (s/or :int nat-int? :string string?))

(s/def ::leaf (s/keys :req [::value ::index]))
(s/def ::parent (s/keys :req [::left ::right ::hash ::value ::index]))
(s/def ::child (s/or :left ::left :right ::right))
(s/def ::node (s/or :parent ::parent :leaf ::leaf))

(s/def ::left ::node)
(s/def ::right ::node)
(s/def ::value (s/or :string string? :int int?))
(s/def ::index nat-int?)

;; test
(sgen/generate (s/gen ::hash))
(sgen/generate (s/gen ::left))
(sgen/generate (s/gen ::node))

;; test
(s/explain ::node {::left {::value 5 ::index 1} ::right {::value 5 ::index 1} ::hash 1 ::value 5 ::index 1})
(s/valid? ::node {::value 1 ::index 1})

(defonce storage (atom {}))
(s/def ::storage-map (s/map-of ::hash ::leaf))
(s/valid? ::storage-map @storage)

(defonce root (atom nil))

(s/fdef reset-storage!
  :ret ::storage-map)
(defn reset-storage! [] (reset! storage {}))

;; test
(let [parent (sgen/generate (s/gen ::parent))]
  [parent (children parent)])

(defn take-index []
  (swap! index inc))

(defn take-leaf-index []
  (swap! leaf-index inc))

(defn decrease-index []
  (swap! index dec))

(defn parentheses-maybe [string]
  (if (nil? (re-find #"⋁" string))
    string
    (str "(" string ")")))

;; (defn retrieve-by-value [])
;; (defn tree-from-storage-map [node]
;;   ())

(defn node [left right index]
  {::left left
   ::right right
   ;; ::value (hash (str left right))
   ::value (if (= :join @labeling)
             (str (parentheses-maybe (str (::value left))) "⋁" (parentheses-maybe (str (::value right))))
             (if (= :set @labeling)
               (let [vals (map ::value [left right])]
                 (if (= ::leaf (::type left))
                   ;; if left is leaf, then so is right
                     (apply sorted-set vals)
                   (if (= ::leaf (::type right))
                     ;; if right is leaf and left not, conj leaf into hashsets
                     (conj (first vals) (second vals))
                     ;; if neither are leaves, concat hashsets
                     (apply clojure.set/union vals))))
               index))
   ::index index
   ::type ::node})

(defn leaf [index & [value]]
  {::value (if value value (if labeling (swap! leaf-index inc) index))
   ::index index
   ::type ::leaf})

;; hack: value of leaf == height of represented peak
(comment (node (leaf 1) (leaf 0) 0))
(S-n 3)

(defn tree-depth [node]
  (if (has-children? node)
    (+ 1
       (apply max (map tree-depth (children node))))
    1))

(defn tree-min-depth [node]
  (if (has-children? node)
    (+ 1
       (apply min (map tree-min-depth (children node))))
    1))

(defn mmr-max-depth [node]
  (if (has-children? node)
    (+ 1
       (mmr-max-depth (::left node)))
    1))

(defn mmr-min-depth [node]
  (if (has-children? node)
    (+ 1
       (mmr-min-depth (::right node)))
    1))

(defn is-peak? [node]
  (and (= :core/node (:core/type node))
       (= (mmr-max-depth node) (mmr-min-depth node))))

(defn hash-node [node]
  (if (has-children? node)
    (hash (str (::left node) (::right node)))
    (hash (::value node))))

(defn storage-add! [node]
  (swap! storage assoc-in [(hash-node node)] node))

(defn is-root? [node])

(defn get-descendants [node]
  (if (has-children? node)
    (conj (children node) (map get-descendants (children node)))))

(defn mmr-leafcount [node]
  (if (nil? node)
    0
    (if (has-children? node)
      (apply + (map mmr-leafcount (children node)))
      1)))

(defn is-power-of-two [num]
  (=
   (.pow (BigInteger. "2") (int (/ (Math/log num) (Math/log 2))))
   num))

(defn mmr-append-leaf [old-node new-leaf]
  (if
   (is-power-of-two (mmr-leafcount old-node))
    ;; if number of leafs stemming from old-node is power of two, then create a new node that has the old-node as left child and the new leaf as right child
    (node old-node new-leaf (take-index))
    ;; if this is not the case, preserve the left branch of the old mmr and append the new leaf to the right branch
    (do (decrease-index) (node (::left old-node) (mmr-append-leaf (::right old-node) (assoc new-leaf ::index @index)) (take-index)))))

(defn mmr-l2r-append-leaf [old-node new-leaf]
  (if
   (is-power-of-two (mmr-leafcount old-node))
    ;; if number of leafs stemming from old-node is power of two, then create a new node that has the old-node as left child and the new leaf as right child
    (node old-node new-leaf (take-index))
    ;; if this is not the case, preserve the left branch of the old mmr and append the new leaf to the right branch
    (do (decrease-index) (node (::left old-node) (mmr-l2r-append-leaf (::right old-node) (assoc new-leaf ::index @index)) (take-index)))))

(defonce root-storage (atom []))

(defn mmb-append-leaf [old-node new-leaf]
  (swap! root-storage conj (leaf (take-index))))

(defn check-subsequency-and-recurse [accumulator remainder]
  (if (empty? remainder)
    (into [] accumulator)
    (let [head (first remainder)]
      (if (= (mmr-leafcount (first head)) (mmr-leafcount (second head)))
        ;; merge-trees-and-fold-rest
        (into [] (concat (conj accumulator (node (first head) (second head) (take-index))) (map first (rest (rest remainder)))))
        (check-subsequency-and-recurse (conj accumulator (first head)) (rest remainder))))))

(defn mmb-from-indexcount [indexcount]
  (reset! index -1)
  (reset! leaf-index -1)
  (reset! root-storage [])
  (reduce (fn [root _]
            (do
              (swap! root-storage conj (leaf (take-index)))
              (swap! root-storage #(check-subsequency-and-recurse [] (partition 2 1 [] %)))))
          []
          (range indexcount))
  @root-storage)

(defn mmr-from-leafcount [leafcount]
  (reset! index -1)
  (reset! leaf-index 0)
  (reduce (fn [root _]
            (mmr-append-leaf root (leaf (take-index) (take-leaf-index))))
          (leaf (take-index) (take-leaf-index))
          (range (dec leafcount))))

(defn range-node? [node]
  (not=
   (mmr-max-depth node)
   (mmr-min-depth node)))

(defn decorate-node-types
  "decorates range and peak nodes"
  [node]
  (if (has-children? node)
    (if (range-node? node)
      (assoc node
             ::left (assoc (::left node) ::type ::peak)
             ::right (decorate-node-types (::right node))
             ::type ::range)
      (assoc node ::type ::peak))
    node))

(defn decorate-node-types-l2r
  "decorates range and peak nodes"
  [node]
  (if (has-children? node)
    (if (range-node? node)
      (assoc node
             ::left (decorate-node-types (::left node))
             ::right (assoc (::right node) ::type ::peak)
             ::type ::range)
      (assoc node ::type ::peak))
    node))

(defn decorate-node [node nodes-to-decorate decoration-map]
  (if (contains? nodes-to-decorate (:index node))
    (merge node decoration-map)
    node))
(tree-min-depth (::right (mmr-from-leafcount 7)))
(tree-depth (::right (mmr-from-leafcount 7)))
(decorate-node-types (mmr-from-leafcount 7))
(defn mmr-graph [root]
  (apply merge (flatten [{((if (= :join @labeling) ::value (if (= :set @labeling) ::value ::index)) root)
                          (map (if (= :join @labeling) ::value (if (= :set @labeling) ::value ::index)) (children root))}
                         (map mmr-graph (children root))])))

(defn mmb-graph [roots]
  (let [mmr-graphs (map mmr-graph roots)
        root-nodes (map ::value roots)]
    (apply merge {"RN-1", root-nodes} mmr-graphs)))

(defn find-subtree
  ([root node-key] (find-subtree root node-key false))
  ([root node-key value?]
   (if (= ((if value? ::value ::index) root) node-key)
     root
     (if (has-children? root)
       (first
        (flatten
         (filter
          #(not (or (nil? %) (empty? %)))
          (map #(find-subtree % node-key value?) (children root)))))))))

(defn keys-in [m]
  (if (map? m)
    (vec
     (mapcat (fn [[k v]]
               (let [sub (keys-in v)
                     nested (map #(into [k] %) (filter (comp not empty?) sub))]
                 (if (seq nested)
                   nested
                   [[k]])))
             m))
    []))

(defn paths [mmr]
  (->> mmr
       keys-in
       (filter #(= :core/type (last %)))
       (filter #(= :core/leaf (get-in mmr %)))
       (map butlast)
       ))

(defn path [mmr leaf]
  (nth (paths mmr) (dec leaf)))

(comment
  (defn copath [mmr leaf]
    (map #(get {:core/left :core/right
                :core/right :core/left}
               %)
         (path mmr leaf))))

(defn path-nodes [root path]
  (if (has-children? root)
    (cons root (path-nodes ((first path) root) (rest path)))
    [root]))

(defn copath-nodes [root path]
  (let [next-copath (((first path) {:core/left :core/right
                                    :core/right :core/left}) root)]
    (if (< 1 (count path))
      (cons next-copath
            (copath-nodes ((first path) root) (rest path)))
      [next-copath])))

(defn find-subtree-mmb [roots node-key & value?]
  ;; (or
  (some identity (map
                  #(find-subtree % node-key value?)
                  roots))
  ;; )
  )

;; mmb visualization
(let [mmb (mmb-from-indexcount 15)
      graph (mmb-graph mmb)]
  (viz/view-graph (keys graph) graph
                  :node->descriptor (fn [n] {:label n})
                  :node->cluster (fn [node-key] (if (= node-key "RN-1")
                                                  0
                                                  (tree-depth (find-subtree-mmb mmb node-key labeling)))))
  graph)

;; mmr visualization

(let [mmr (mmr-from-leafcount 4)
      graph (mmr-graph mmr)]
  (viz/view-graph (keys graph) graph
                  :node->descriptor (fn [n] {:label n})
                  :node->cluster (fn [node-key] (tree-depth (find-subtree mmr node-key labeling))))
  graph)

(defn rhizome-to-tangle [graph]
  [(keys graph)
   (apply concat (map (fn [[k v]] (map #(identity [k %]) v)) graph))
   ;; {}
   {:node {:shape :oval}
    :node->id (fn [n] (if (keyword? n) (name n) n))
    ;; :node->descriptor (fn [n] (when-not (keyword? n) n))
    }])

(defn tangle-view [graph]
  (->
   graph
   rhizome-to-tangle
   (#(apply tangle/graph->dot %))
   (tangle/dot->image "png")
   javax.imageio.ImageIO/read
   viz/view-image))

(tangle-view (mmb-graph (mmb-from-indexcount 9)))

(mmb-graph (mmb-from-indexcount 3))

(def html-node {:id "html" :color "blue" :label [:TABLE {:BORDER 0} [:TR [:TD "hic"] [:TD {:BORDER 1} "cup"]]]})

(def nodes [:a :b :c :d html-node])

(def edges [[:a :b] [:a :c] [:c :d] [:a :c {:label "another" :style :dashed}] [:a :html]])

(def dot (tangle/graph->dot nodes edges {:node {:shape :oval}
                                         :node->id (fn [n] (if (keyword? n) (name n) (:id n)))
                                         :node->descriptor (fn [n] (when-not (keyword? n) n))}))

(defn view-dot [graph]
  (viz/view-image (javax.imageio.ImageIO/read (tangle/dot->image graph "png"))))

(view-dot dot)

(comment
  (mmr-leafcount (::left (mmr-from-leafcount 14)))
  (mmr-leafcount (::right (mmr-from-leafcount 14)))

  (mmr-leafcount (::left (::left (mmr-from-leafcount 14))))
  (mmr-leafcount (::right (::left (mmr-from-leafcount 14)))))

(def example-mmr
  (do
    (reset! index -1)
    (reset! leaf-index -1)
    (node
     (node (leaf 0) (leaf 1) (take-index))
     (node (leaf 3) (leaf 4) (take-index))
     (take-index))))

(def extended-mmr (reduce (fn [root leaf-label] (mmr-append-leaf root (leaf leaf-label))) example-mmr [5 6 7 8]))
(identity example-mmr)
(identity extended-mmr)

(tangle-view (mmr-graph example-mmr))
(tangle-view (mmr-graph (mmr-append-leaf example-mmr (leaf 5))))
(tangle-view (mmr-graph extended-mmr))

;; (let [example-belt
;;       (node (node (leaf 0) (leaf 1) 0) (leaf 2) 1)]
;;   (belt-depth-right-most example-belt))

;; (defn replace-right-most [node new-leaf]
;;   )

(def example-belt (node (node (leaf 0) (leaf 1) 0) (leaf 2) 1))

(def example-belt-2 (node (node (leaf 0) (leaf 1) 0) (node (leaf 2) (leaf 3) 2) 1))

(mmb-append-leaf example-belt (leaf 3))
;; (tangle-view (mmr-graph (mmb-from-indexcount 9)))

(tangle-view (mmb-graph (mmb-from-indexcount 10)))

(mmr-graph example-belt)

(defn belt-depth [node]
  (if (has-children? node)
    (+ 1
       (apply max (map belt-depth (children node))))
    0))

(let [example-belt
      (node (node (leaf 0) (leaf 1) 0) (leaf 2) 1)]
  (belt-depth example-belt))

(defn belt-depth-right-most [node]
  (if (has-children? node)
    (+ 1
       (belt-depth-right-most (:core/right node)))
    0))

(defn belt-child-right-most [node]
  (let [depth (belt-depth-right-most node)]
    (nth (iterate :core/right node) depth)))

(belt-child-right-most example-belt-2)

(belt-depth-right-most example-belt)
(:core/right example-belt)

(defn mmb-append-leaf [old-node new-leaf]
  (if
   (not (has-children? old-node))
    (node old-node new-leaf (take-index))
    ;; if this is not the case, preserve the left branch of the old mmr and append the new leaf to the right branch
    ;; (do (decrease-index) (node (::left old-node) (mmr-append-leaf (::right old-node) (assoc new-leaf ::index @index)) (take-index)))
    (node (::left old-node) (mmb-append-leaf (::right old-node) new-leaf) (take-index))))

;; (mmb-append-leaf (mmb-from-indexcount 3) (leaf 3))

(defn belt-right-most-operators [tree]
  (let [sequence-length (belt-depth-right-most tree)]
    {::right-most-child
     (into [] (repeat sequence-length ::right))
     ::sibling-of-right-most-child
     (conj (into [] (repeat (dec sequence-length) ::right)) ::left)
     ::parent-of-right-most-child
     (into [] (repeat (dec sequence-length) ::right))
     ;; tree
     ;; ::right-most-child
     ;; (get-in tree (repeat sequence-length ::right))
     ;; ::sibling-of-right-most-child
     ;; (get-in tree (conj (repeat (dec sequence-length) ::right) ::left))
     ;; ::parent-of-right-most-child
     ;; (get-in tree (repeat (dec sequence-length) ::right))
     ;; ;; tree
     }))

;; reattempt at appending leafs:
;; we have two layers of storage: the distinct merkle trees (vector based storage)

(let [leaf-count 3
      S-n (primitives.core/S-n leaf-count)
      right-most-depth (last S-n)
      ;; example-tree (node (node (leaf 0) (leaf 1) 0) (leaf 2) 1)
      example-tree (mmr-from-leafcount 3)]
  [(tangle-view (mmr-graph example-tree))
   (tangle-view (mmr-graph (if (is-power-of-two (inc leaf-count))
                             ;; append only if leaf-count + 1 is power of two (look at S-n to understand)
                             (let [;; new-rightmost is a node with the former rightmost as left-child and a new leaf as the right child
                                   new-rightmost (node (belt-child-right-most example-tree) (leaf 20) 99)]
                               (assoc-in example-tree (::right-most-child (belt-right-most-operators example-tree)) new-rightmost))
                             ;; append and merge
                             (let [;; new-rightmost is a node with the former rightmost as left-child and a new leaf as the right child
                                   new-rightmost (node (belt-child-right-most example-tree) (leaf 10) 99)
                                   ;; append-step
                                   new-tree (assoc-in example-tree (::right-most-child (belt-right-most-operators example-tree)) new-rightmost)
                                   ;; merge-step
                                   merged-tree ()]
                               new-tree))))
   ;; (belt-child-right-most example-tree)
   ;; (::right-most-child (belt-right-most-operators example-tree))
   ;; right-most-depth
   ;; S-n
   ;; example-tree
   ])

(mmr-graph (mmr-from-leafcount 2))

;; new idea: create just the belt with the S-n as the effective leaves
;; thereafter, map parent-less-nodes to effective leaves
;; ()

(let [example-tree (mmr-from-leafcount 90)]
  (belt-depth-right-most example-tree))

;; (get-in (mmr-from-leafcount 90) (::sibling-of-right-most-child (belt-right-most-operators (mmr-from-leafcount 90))))

(mmb-from-indexcount 3)
#:core{:left #:core{:value 0, :index 0}, :right #:core{:value 1, :index 1}, :value 0, :index 0}

(second (mmr-from-leafcount 3))

(defn deep-walk
  "replace `value`s in nested `data` structure by (`f` `value`)"
  [f data]
  (vec (map #(if (coll? %)
               (deep-walk f %)
               (f %)) data)))

;; NOTE: fucking ugly xD
;; TODO: unuglify ;)
(defn convert-nested-to-indices
  "takes a nested data structure and replaces all the entries with their index, if the entries were walked"
  [nested]
  (let [indexer (atom -1)]
    (deep-walk (fn [_] (swap! indexer inc)) nested)))

(defn find-index-route [x coll]
  (letfn [(path-in [y]
            (cond
              (= y x) '()
              (coll? y) (let [[failures [success & _]]
                              (->> y
                                   (map path-in)
                                   (split-with not))]
                          (when success (cons (count failures) success)))))]
    (path-in coll)))

(letfn [(discrepancy-at-index [index seqs]
          (apply not= (map #(nth % index nil) seqs)))
        (belt-ranges-index-routes [index n]
          (find-index-route index (convert-nested-to-indices (belt-ranges n))))
        ;; (belt-ranges-routes [])
        (belt-ranges-routes [m]
          ;; #dbg
          (map conj
               (map (fn [index] (belt-ranges-index-routes index m)) (range ((comp count flatten belt-ranges) m)))
               ;; (flatten (belt-ranges m))
               (primitives.core/S-n m)))]

  (comment
    (println "-------------------"
             (println
              (map (fn [n] (filter #(discrepancy-at-index % [(primitives.core/S-n n) (primitives.core/S-n (inc n))])
                                   (range ((comp count primitives.core/S-n inc) n))))
                   (range 30)))
             (println
              (map (fn [n] (filter #(discrepancy-at-index % (map belt-ranges-routes [n (inc n)]))
                                   (range ((comp count flatten belt-ranges inc) n))))
                   (range 30)))))
  (let [maxn 1000]
    (=
     (pmap (fn [n] (filter #(discrepancy-at-index % [(primitives.core/S-n n) (primitives.core/S-n (inc n))])
                           (range ((comp count primitives.core/S-n inc) n))))
           (range maxn))
     (pmap (fn [n] (filter #(discrepancy-at-index % (map belt-ranges-routes [n (inc n)]))
                           (range ((comp count flatten belt-ranges inc) n))))
           (range maxn))))
  ;; NOTE ergo: locations of changes in in belt-range splitting are only a subset of the locations of changes in S-n
  )

(letfn [(discrepancy-at-index [index seqs]
          (apply not= (map #(nth % index nil) seqs)))
        (belt-ranges-index-routes [index n]
          (find-index-route index (convert-nested-to-indices (belt-ranges n))))
        ;; (belt-ranges-routes [])
        (belt-ranges-routes [m]
          ;; #dbg
          (map conj
               (map (fn [index] (belt-ranges-index-routes index m)) (range ((comp count flatten belt-ranges) m)))
               ;; (flatten (belt-ranges m))
               (primitives.core/S-n m)))]

  (let [n 1278]
    [;; returns the index at which first discrepancy is detected
     ((fn [n] (first (filter #(discrepancy-at-index % [(primitives.core/S-n n) (primitives.core/S-n (inc n))])
                             (range ((comp count primitives.core/S-n inc) n)))))
      n)
     ;; returns the belt ranges
     (comment
       ((fn [m]
          (map conj
               (map (fn [index] (belt-ranges-index-routes index m)) (range ((comp count flatten belt-ranges) m)))
               ;; (flatten (belt-ranges m))
               (primitives.core/S-n m)))
        n))
     (belt-ranges-routes n)]))

(map
 (fn [n] (filter
          (fn [index] (apply not= (map #(nth % index nil) ((juxt primitives.core/S-n (comp primitives.core/S-n inc)) n))))
          (range ((comp count primitives.core/S-n inc) n))))
 (range 30))

(comment
  ((fn [index n]
     (find-index-route index (convert-nested-to-indices (belt-ranges n)))) 0 2))

(map convert-nested-to-indices [(belt-ranges 50) (belt-ranges 51)])

(map
 (fn [belt-range] (map
                   #(find-index-route % (convert-nested-to-indices belt-range))
                   (range (count (flatten belt-range)))))
 [(belt-ranges 2) (belt-ranges 3)])

(belt-ranges 50)
(belt-ranges 51)

(letfn
 [(discrepancy-at-index [seqs]
    (let [max-index (apply max (map count seqs))]
      (filter (fn [index] (apply not= (map #(nth % index nil) seqs))) (range max-index))))]

  (let [a (let [n 27]
            (map conj
                 (map (fn [index] (find-index-route index (convert-nested-to-indices (belt-ranges n)))) (range (count (flatten (belt-ranges n)))))
                 (flatten (belt-ranges n))))
        b (let [n 28]
            (map conj
                 (map (fn [index] (find-index-route index (convert-nested-to-indices (belt-ranges n)))) (range (count (flatten (belt-ranges n)))))
                 (flatten (belt-ranges n))))]
    (discrepancy-at-index [a b])))

(let [n 27]
  (map (fn [index] (find-index-route index (convert-nested-to-indices (belt-ranges n)))) (range (count (flatten (belt-ranges n))))))
(map conj
     (let [n 28]
       (map (fn [index] (find-index-route index (convert-nested-to-indices (belt-ranges n)))) (range (count (flatten (belt-ranges n))))))
     (flatten (belt-ranges 28)))

(map conj [[1 2 3] [5 6 7]] [4 8])
;; (find-index-route 0 [1 0 0])

;; (letfn [(mismatch-in-primitives.core/S-n-at-index [index n]
;;           (apply not= (map #(nth % index nil) ((juxt primitives.core/S-n (comp primitives.core/S-n inc)) n))))]
;;   (map
;;    (fn [n] (filter
;;            #(mismatch-in-primitives.core/S-n-at-index % n)
;;            (range ((comp count primitives.core/S-n inc) n))))
;;    (range 30)))

;; map indices to belt-ranges
(comment
  (belt-ranges @primitives.storage/leaf-count))

((juxt primitives.core/S-n belt-ranges) 4)

(comment
  (primitives.storage/range-node-edges (first (belt-ranges 1222)) 3))
;; (primitives.storage/bag-left-to-right (into [] (map storage/bag-left-to-right (belt-ranges 1222))))

(defonce parent-less-nodes-remainder (atom @primitives.storage/parent-less-nodes-cache))
(defn take-parent-less-node []
  (let [first-parent-less (first @parent-less-nodes-remainder)]
    (swap! parent-less-nodes-remainder rest)
    first-parent-less))
(comment
  (take-parent-less-node))

;; (def belt-node-index (atom 0))

(defn ordering-peaks []
  (map first (primitives.storage/parent-less-nodes-sorted-height @primitives.storage/parent-less-nodes-cache)))

(defn position-in-peak-ordering [peak-index]
  (let [ordering-peaks (ordering-peaks)] (first (filter #(= peak-index (nth ordering-peaks %)) (-> ordering-peaks count range)))))

(defn range-aggregator
  "takes ranges and produces lists of the edges that represent these ranges together with belt nodes"
  [ranges]
  (do
    (reset! parent-less-nodes-remainder (map first (primitives.storage/parent-less-nodes-sorted-height @primitives.storage/parent-less-nodes-cache)))
    (reduce (fn
              [[range-collector starting-index belt-node-index] new-range]
              (let [[new-edges range-nodes last-index] (primitives.storage/range-node-edges new-range starting-index)]
                [;; updated range collector
                 (concat
                  ;; take current collection of ranges
                  range-collector
                  ;; add newly concatenated list
                  (if (not (empty? new-edges)) new-edges)
                  ;; and add an edge between the last range's range-node and the next ranges range-node
                  ;; [[(str "range-node-" last-index) (str "range-node-" (inc last-index))]]
                  ;; add a new belt node n, with the edges
                  ;; [belt-node-n-1 belt-node-n] [last-node belt-node-n] or
                  ;; [last-node-n-1 belt-node-0] [last-node-n belt-node-0], whichever is applicable
                  (if (not (= -2 belt-node-index))
                    (let [last-belt-node {:type "belt-node" :index belt-node-index}
                          new-belt-node {:type "belt-node" :index (inc belt-node-index)}]
                      (if (= -1 belt-node-index)
                        ;; if first belt node, add the edges [last-node-n-1 belt-node-0] [last-node-n belt-node-0]
                        [[(last (last range-collector)) new-belt-node] [(if (empty? new-edges)
                                                                          ;; if no new edges, then the last range was a singleton, so we just append it directly to the belt
                                                                          (first new-range)
                                                                          (last (last new-edges)))
                                                                        new-belt-node]]
                        ;; otherwise, add the edges [belt-node-n-1 belt-node-n] [last-node belt-node-n]
                        [[last-belt-node new-belt-node] [(if (empty? new-edges)
                                                           ;; if no new edges, then the last range was a singleton, so we just append it directly to the belt
                                                           (first new-range)
                                                           (last (last new-edges))) new-belt-node]]))))

                 ;; updated starting-index

                 (if (empty? range-nodes)
                   last-index
                   (inc last-index))

                 ;; updated belt-node-index
                 (inc belt-node-index)]))
            ;; start index of belt node at -2 to have `new-belt-node` at 0 once where at the second range
            [[] 0 -2]
            ranges)))

(comment
  #_{:clj-kondo/ignore [:invalid-arity]}
  ([0 0 1] [1] [0 0 0 1] [1 1]))
(comment
  #_{:clj-kondo/ignore [:invalid-arity]}
  ([0 "range-node-0"] [0 "range-node-0"] ["range-node-0" "range-node-1"] [1 "range-node-1"] [1 "belt-node"] [0 "range-node-2"] [0 "range-node-2"] ["range-node-2" "range-node-3"] [0 "range-node-3"] ["range-node-3" "range-node-4"] [1 "range-node-4"] [1 "range-node-5"] [1 "range-node-5"]))
(first (range-aggregator (belt-ranges 1222)))

(comment
  (identity primitives.storage/parent-less-nodes-cache))

(primitives.core/bits-of-n 4)

(identity @parent-less-nodes-remainder)
;; TODO: fix broken walk
(deep-walk (fn [_] (take-parent-less-node)) (belt-ranges 1222))
(first (range-aggregator (deep-walk (fn [_] (take-parent-less-node)) (belt-ranges 1222))))

(comment
  (primitives.storage/node-name 1536))

;; TODO: this is maybe not the right place to do this - perhaps only once creating graph?
(defn parse-typed-name [node]
  (str (:type node) "-" (:index node)))

;; map indices to node names
(defn belted-edges
  "map indices of peaks of an mmb to their node-names"
  []
  (do
    (reset! parent-less-nodes-remainder (map first (primitives.storage/parent-less-nodes-sorted-height @primitives.storage/parent-less-nodes-cache)))
    (map (fn [[child parent]]
           [(if (int? child)
              (primitives.storage/node-name child)
              ;; (#(str (first %) ": " (second %)) ((juxt identity storage/node-name storage/node-height-literal) child))
              ;; child
              (parse-typed-name child))
            (parse-typed-name parent)])
         (first (range-aggregator (deep-walk (fn [_] (take-parent-less-node)) (belt-ranges @primitives.storage/leaf-count)))))))

(defn belted-nodes
  "map indices of peaks of an mmb to their node-names"
  []
  (do
    (reset! parent-less-nodes-remainder (map first (primitives.storage/parent-less-nodes-sorted-height @primitives.storage/parent-less-nodes-cache)))
    (map (fn [[child parent]]
           [(if (= "peak-node" (:type child))
              {:node-height (primitives.storage/node-height-literal (:index child))
               :id (str (:type child) "-" (:index child))
               ;; :id (#(str (first %) ": " (second %)) ((juxt storage/node-name storage/node-height-literal) (:index child)))
               :index (:index child)
               ;; :pos (str child "," (primitives.storage/node-height-literal child))}
               ;; :pos (str child "," 0)}
               :pos 0}
              ;; child
              {:id (str (:type child) "-" (:index child))
               :index (:index child)
               :pos (if (= "range-node" (:type child)) 1 2)})
            {:id (str (:type parent) "-" (:index parent))
             :index (:index parent)
             :pos (if (= "range-node" (:type parent)) 1 2)}])
         (first (range-aggregator (deep-walk (fn [_] ((fn [node] {:type "peak-node" :index node}) (take-parent-less-node))) (belt-ranges @primitives.storage/leaf-count)))))))

(comment
  (primitives.storage/node-maps (into [] (flatten (belted-edges)))))
(comment
  (primitives.storage/node-maps (into [] (flatten (belted-nodes)))))

(defn update-position
  "takes an index ordered list of maps describing a point and updates the pos to use the index order as the x component of the position"
  [peaks]
  (map (fn [posx m] (update-in m [:pos] (fn [posy] (str (* 2.2 posx) "," posy "!")))) (range (count peaks)) peaks))

(defn peak-x-positions [peaks]
  (map (fn [posx m] (merge m {:posx posx})) (range (count peaks)) peaks))

(defn merge-positions
  ;; "takes an index ordered list of maps describing a point and updates the pos to use the index order as the x component of the position"
  [peak]
  (merge peak {:pos (str (:posx peak) "," (:posy peak) "!")}))

;; update position based on right-most child
(defn right-most-child-edge [edge-name]
  (first (second (filter #(= edge-name (second %)) (belted-edges)))))

;; TODO: the above can definitely be improved upon - maybe instead of a vectorized approach, return to nesting like before?
;; for the volatile nodes (ranges and belts), it may make sense to have relatively complex structures, since there are few of them

(right-most-child-edge "range-node-0")

;;TODO: rip apart `update-position` - rather save posx & posy separately and merge thereafter
;; (defn update-position [])

(def test-nodes (let [nodes (group-by :type
                                      (map #(merge % {:id (if (= "peak-node" (:type %))
                                                            (primitives.storage/node-name (:index %))
                                                            (str (:type %) "-" (:index %)))
                                                      :posy (get {"peak-node" 0,
                                                                  "range-node" 1,
                                                                  "belt-node" 2} (:type %))})
                                           (into #{} (flatten
                                                      (first (range-aggregator
                                                              (deep-walk (fn [_] ((fn [node] {:type "peak-node" :index node}) (take-parent-less-node)))
                                                                         (belt-ranges @primitives.storage/leaf-count))))))))]
                  [(sort-by :index (fn [x y] (compare
                                              (position-in-peak-ordering x)
                                              (position-in-peak-ordering y))) (get nodes "peak-node"))
                   (get nodes "range-node")
                   (get nodes "belt-node")
                   ;; (update-position (sort-by :index (get nodes "range-node")))
                   ;; (update-position (sort-by :index (get nodes "belt-node")))
                   ]))

(def test-nodes-decorated
  (let [peak-nodes (map merge-positions (peak-x-positions (first test-nodes)))
        range-nodes (sort-by :index (second test-nodes))
        belt-nodes (sort-by :index (nth test-nodes 2))
        range-nodes-decorated (map (fn [parent-node] (merge parent-node {:posx (:posx (first (filter #(= (right-most-child-edge (:id parent-node)) (:id %)) peak-nodes)))})) range-nodes)
        belt-nodes-decorated (map (fn [parent-node] (merge parent-node {:posx (:posx (first (filter #(= (right-most-child-edge (:id parent-node)) (:id %)) (concat peak-nodes range-nodes-decorated))))})) belt-nodes)]

    (concat
     peak-nodes
     range-nodes-decorated
     belt-nodes-decorated)
    ;; (map #(right-most-child-edge (:id %)) range-nodes)
    ;; range-nodes
    ))

(comment
  (map merge-positions (map #(update % :posx (fn [old] (* 2 old))) test-nodes-decorated)))

(defn graph-nodes []
  (let [nodes (group-by :type
                        (map #(merge % {:id (if (= "peak-node" (:type %))
                                              (primitives.storage/node-name (:index %))
                                              (str (:type %) "-" (:index %)))
                                        :posy (get {"peak-node" 0,
                                                    "range-node" 1,
                                                    "belt-node" 2} (:type %))})
                             (into #{} (flatten
                                        (first (range-aggregator
                                                (deep-walk (fn [_] ((fn [node] {:type "peak-node" :index node}) (take-parent-less-node)))
                                                           (belt-ranges @primitives.storage/leaf-count))))))))]
    (concat
     (update-position (sort-by :index (fn [x y] (compare
                                                 (position-in-peak-ordering x)
                                                 (position-in-peak-ordering y))) (get nodes "peak-node")))
     (update-position (sort-by :index (get nodes "range-node")))
     (update-position (sort-by :index (get nodes "belt-node"))))))

(sort-by :index (fn [x y] (compare (* -1 x) y)) [{:index -7} {:index 0}])

(map #(select-keys % [:id :pos :index]) (graph-nodes))

;; (ordering-peaks)

(re-matches #"range-node.*" "range-node-0")

(comment (primitives.storage/node-maps (into [] (flatten (belted-edges)))))
(group-by :type (into [] (into #{} (flatten (belted-nodes)))))

(first (range-aggregator (belt-ranges 4)))

(comment
  #_{:clj-kondo/ignore [:invalid-arity]}
  ([0 "range-node-0"]
   [0 "range-node-0"]
   ["range-node-0" "range-node-1"]
   [1 "range-node-1"]
   ["range-node-1" "belt-node-0"]
   [1 "belt-node-0"]
   [0 "range-node-2"]
   [0 "range-node-2"]
   ["range-node-2" "range-node-3"]
   [0 "range-node-3"]
   ["range-node-3" "range-node-4"]
   [1 "range-node-4"]
   ["belt-node-0" "belt-node-1"]
   ["range-node-4" "belt-node-1"]
   [1 "range-node-5"]
   [1 "range-node-5"]
   ["belt-node-1" "belt-node-2"]
   ["range-node-5" "belt-node-2"]))

;; (defn ranges)
(comment
  (primitives.storage/range-node-edges '[1] 1))

(+ 4 4)

(println "end:" (new java.util.Date))
