(ns core
  (:require [rhizome.viz :as viz]
            [tangle.core :as tangle]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]))

(def index (atom -1))
(def leaf-index (atom -1))

(def join-labeling false)

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

(s/def ::storage-map (s/map-of ::hash ::leaf))
(defonce storage (atom {}))
(s/valid? ::storage-map @storage)

(def root (atom nil))

(s/fdef reset-storage!
  :ret ::storage-map)
(defn reset-storage! [] (reset! storage {}))

(defn children [node]
  (into [] (filter some?
           ((juxt ::left ::right) node))))

(defn has-children? [node]
  (not-empty (children node)))

(s/fdef children
  :args (s/cat :node ::node)
  ;; :ret (s/tuple ::parent (s/coll-of ::child))
  :ret (s/? (s/cat :left ::left :right ::right))
  :fn #(if (has-children? (second (:node (:args %))))
         (= 2 (count (:ret %)))
         ;; (do
         ;;   (if (not= 0 (count (:ret %))) (println %))
         ;;  true)
         (= 0 (count (:ret %)))
         )
  )

(stest/instrument `children)
(stest/check `children)

;; test
(let [parent (sgen/generate (s/gen ::parent))]
  [parent (children parent)])

(defn hash-node [node]
  (if (has-children? node)
    (hash (str (::left node) (::right node)))
    (hash (::value node))))

(defn take-index []
  (swap! index inc))

(defn decrease-index []
  (swap! index dec))

(defn parentheses-maybe [string]
  (if (nil? (re-find #"⋁" string))
    string
    (str "(" string ")")))

(defn retrieve-by-value [])
(defn tree-from-storage-map [node]
  ())

(defn node [left right index]
  {::left left
   ::right right
   ;; ::value (hash (str left right))
   ::value (if join-labeling
             (str (parentheses-maybe (str (::value left))) "⋁" (parentheses-maybe (str (::value right))))
             index)
   ::index index}
  )

;; (mmr-from-leafcount 5)

(defn leaf [index & value]
  {::value (if value value (if join-labeling (swap! leaf-index inc) index))
   ::index index})

(defn mmr-depth [node]
  (if (has-children? node)
     (+ 1
        (apply max (map mmr-depth (children node))))
     1)
    )

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

(defonce root-storage (atom []))

(defn mmb-append-leaf [old-node new-leaf]
  (swap! root-storage conj (leaf (take-index)))
  )

(defn check-subsequency-and-recurse [accumulator remainder]
  (if (empty? remainder)
    (into [] accumulator)
    (let [head (first remainder)]
      (if (= (mmr-leafcount (first head)) (mmr-leafcount (second head)))
        ;; merge-trees-and-fold-rest
        (into [] (concat (conj accumulator (node (first head) (second head) (take-index))) (map first (rest (rest remainder)))))
        (check-subsequency-and-recurse (conj accumulator (first head)) (rest remainder))
        ))
    )
  )

(defn mmb-from-indexcount [indexcount]
  (reset! index -1)
  (reset! leaf-index -1)
  (reset! root-storage [])
  (reduce (fn [root _]
            (do
              (swap! root-storage conj (leaf (take-index)))
              (swap! root-storage #(check-subsequency-and-recurse [] (partition 2 1 [] %)))
              ))
          []
          (range indexcount)
          )
  @root-storage
  )

(defn mmr-from-leafcount [leafcount]
  (reset! index -1)
  (reset! leaf-index -1)
  (reduce (fn [root _]
            (mmr-append-leaf root (leaf (take-index))))
          (leaf (take-index))
          (range (dec leafcount))))

(defn mmr-graph [root]
  (apply merge (flatten [{
                          ((if join-labeling ::value ::index) root)
                          (map (if join-labeling ::value ::index) (children root))}
                         (map mmr-graph (children root))])))

(defn mmb-graph [roots]
  (let [mmr-graphs (map mmr-graph roots)
        root-nodes (map ::value roots)
        ]
    (apply merge {"RN-1", root-nodes} mmr-graphs)
    ))

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

(defn find-subtree-mmb [roots node-key & value?]
  ;; (or
   (some identity (map
                   #(find-subtree % node-key value?)
                   roots
                   ))
   ;; )
  )

;; mmb visualization
(let [mmb (mmb-from-indexcount 15)
      graph (mmb-graph mmb)]
  (viz/view-graph (keys graph) graph
                  :node->descriptor (fn [n] {:label n})
                  :node->cluster (fn [node-key] (if (= node-key "RN-1")
                                                 0
                                                 (mmr-depth (find-subtree-mmb mmb node-key join-labeling))))
                  )
  graph
  )

;; mmr visualization
(let [mmr (mmr-from-leafcount 4)
      graph (mmr-graph mmr)]
  (viz/view-graph (keys graph) graph
                  :node->descriptor (fn [n] {:label n})
                  :node->cluster (fn [node-key] (mmr-depth (find-subtree mmr node-key join-labeling)))
                  )
  graph
  )

(defn rhizome-to-tangle [graph]
  [
   (keys graph)
   (apply concat (map (fn [[k v]] (map #(identity [k %]) v)) graph))
   ;; {}
   {:node {:shape :oval}
    :node->id (fn [n] (if (keyword? n) (name n) n))
    ;; :node->descriptor (fn [n] (when-not (keyword? n) n))
    }
   ])

(defn tangle-view [graph]
  (->
   graph
   rhizome-to-tangle
   (#(apply tangle/graph->dot %))
   (tangle/dot->image "png")
   javax.imageio.ImageIO/read
   viz/view-image
   ))

(tangle-view (mmb-graph (mmb-from-indexcount 3)))

(mmb-graph (mmb-from-indexcount 3))

(def html-node {:id "html" :color "blue" :label [:TABLE {:BORDER 0} [:TR [:TD "hic"] [:TD {:BORDER 1} "cup"]]]})

(def nodes [:a :b :c :d html-node])

(def edges [[:a :b] [:a :c] [:c :d] [:a :c {:label "another" :style :dashed}] [:a :html]])

(def dot (tangle/graph->dot nodes edges {:node {:shape :oval}
                                  :node->id (fn [n] (if (keyword? n) (name n) (:id n)))
                                  :node->descriptor (fn [n] (when-not (keyword? n) n))
                                  }))

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
(defn belt-depth [node]
  (if (core/has-children? node)
    (+ 1
       (apply max (map belt-depth (core/children node))))
    0)
  )


(defn belt-depth-right-most [node]
  (if (core/has-children? node)
    (+ 1
       (belt-depth-right-most (:core/right node)))
    0)
  )

(defn belt-child-right-most [node]
  (let [depth (belt-depth-right-most node)]
      (nth (iterate :core/right node) depth)))
(defn mmb-append-leaf [old-node new-leaf]
  (if
      (not (has-children? old-node))
    (node old-node new-leaf (take-index))
    ;; if this is not the case, preserve the left branch of the old mmr and append the new leaf to the right branch
    ;; (do (decrease-index) (node (::left old-node) (mmr-append-leaf (::right old-node) (assoc new-leaf ::index @index)) (take-index)))
    (node (::left old-node) (mmb-append-leaf (::right old-node) new-leaf) (take-index))
    ))

(defn binary-repr-of-n [n]
  (Integer/toBinaryString n))

(defn bits-of-n [n]
  (map (comp #(Integer. %) str) (binary-repr-of-n n)))

(defn bits-of-inc-n [n]
  (bits-of-n (inc n)))

(defn S-n [n]
  (let [bits (bits-of-inc-n n)
        reversed-bits (reverse bits)]
    (reverse (map
              #(+ % (nth reversed-bits %))
              (range (dec (count bits)))))))

;; hack: value of leaf == height of represented peak
(node (leaf 1) (leaf 0) 0)
(S-n 3)

(defn belt-right-most-operators [tree]
  (let [sequence-length (belt-depth-right-most tree)]
    {
     ::right-most-child
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
     }
    )
  )

;; reattempt at appending leafs:
;; we have two layers of storage: the distinct merkle trees (vector based storage)
(let [
      leaf-count 3
      S-n (S-n leaf-count)
      right-most-depth (last S-n)
      ;; example-tree (node (node (leaf 0) (leaf 1) 0) (leaf 2) 1)
      example-tree (mmr-from-leafcount 3)
      ]
  [
   (tangle-view (mmr-graph example-tree))
   (tangle-view (mmr-graph (if (is-power-of-two (inc leaf-count))
                             ;; append only if leaf-count + 1 is power of two (look at S-n to understand)
                             (let [;; new-rightmost is a node with the former rightmost as left-child and a new leaf as the right child
                                   new-rightmost (node (belt-child-right-most example-tree) (leaf 20) 99)]
                               (assoc-in example-tree (::right-most-child (belt-right-most-operators example-tree)) new-rightmost)
                               )
                             ;; append and merge
                             (let [;; new-rightmost is a node with the former rightmost as left-child and a new leaf as the right child
                                   new-rightmost (node (belt-child-right-most example-tree) (leaf 10) 99)
                                   ;; append-step
                                   new-tree (assoc-in example-tree (::right-most-child (belt-right-most-operators example-tree)) new-rightmost)
                                   ;; merge-step
                                   merged-tree ()
                                   ]
                               new-tree
                               )
                             )))
   ;; (belt-child-right-most example-tree)
   ;; (::right-most-child (belt-right-most-operators example-tree))
   ;; right-most-depth
   ;; S-n
   ;; example-tree
   ]
)


;; new idea: create just the belt with the S-n as the effective leaves
;; thereafter, map parent-less-nodes to effective leaves
;; ()

(let [example-tree (mmr-from-leafcount 90)]
  (belt-depth-right-most example-tree))

;; (get-in (mmr-from-leafcount 90) (::sibling-of-right-most-child (belt-right-most-operators (mmr-from-leafcount 90))))
;; fresh attempt (16.11.2021)
;; reproduce the bagging structure by always recalculating from scratch. this is the first step in iteration towards moving towards a cached model

(defn create-new-range? [[n-2 n-1] n]
  (or (and (= 1 n-1) (= 0 n))
      (and (= 0 n-2) (= 1 n-1))))

(let [
      running-range [[]]
      ;; running-range [[0 0 1] [1] [0 0 1]]
      ;; running-range [[0 0 1] [1] [0 0 1] [0 0]]
      flattened-running-range (flatten running-range)
      new-bit 0]
  (if (create-new-range? (drop (- (count flattened-running-range) 2) flattened-running-range) new-bit)
    ;; if true, create new range
    (conj (into [] running-range) [new-bit])
    ;; else, append to last current range
    (concat (drop-last running-range) [(conj (into [] (last running-range)) new-bit)])
    )
   )

(defn append-to-belt-range [running-range new-bit]
  (let [flattened-running-range (flatten running-range)]
    (if (create-new-range? (drop (- (count flattened-running-range) 2) flattened-running-range) new-bit)
      ;; if true, create new range
      (conj (into [] running-range) [new-bit])
      ;; else, append to last current range
      (concat (drop-last running-range) [(conj (into [] (last running-range)) new-bit)])
      )
    ))
(append-to-belt-range [[0 0 1] [1] [0 0 1]] 0)

(bits-of-inc-n 10)
(defn belt-ranges [n]
  (reduce append-to-belt-range [[]] (rest (bits-of-n n))))

;; map indices to belt-ranges
(belt-ranges @storage/leaf-count)

