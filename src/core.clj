(ns core
  (:require [rhizome.viz :as viz]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]))

(def index (atom -1))
(def leaf-index (atom -1))

(def join-labeling true)

(s/def ::hash int?)

(s/def ::left ::hash)
(s/def ::right ::hash)
(s/def ::value int?)
(s/def ::index nat-int?)

(s/def ::leaf (s/keys :req [::value ::index]))
(s/def ::parent (s/keys :req [::left ::right ::hash ::value ::index]))
(s/def ::node (s/or :parent ::parent :leaf ::leaf))

;; test
(sgen/generate (s/gen ::hash))
(sgen/generate (s/gen ::left))
(sgen/generate (s/gen ::node))

;; test
(s/valid? ::node {::left 1 ::right 1 ::hash 1 ::value 1 ::index 1})
(s/valid? ::node {::value 1 ::index 1})

(s/def ::storage-map (s/map-of ::hash ::leaf))
(defonce storage (atom {}))
(s/valid? ::storage-map @storage)

(def root (atom nil))

(s/fdef reset-storage!
  :ret ::storage-map)
(defn reset-storage! [] (reset! storage {}))

(reset-storage!)
(def temp-storage @storage)

(defn children [node]
  (filter some?
          ((juxt ::left ::right) node)))

;; test
(let [parent (sgen/generate (s/gen ::parent))]
  [parent (children parent)])

(defn has-children? [node]
  (not-empty (children node)))

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
   ;; ::value (str (::value left) "⋁" (::value right))
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
    1))

(defn storage-add! [node]
  (swap! storage assoc-in [(hash-node node)] node))

(defn is-root? [node])
(defn get-descendants [node]
  (if (has-children? node)
    (conj (children node) (map get-descendants (children node)))))

;; (get-descendants)
(identity @storage)

(storage-add! (leaf 2))
(storage-add! (node (leaf 1) (leaf 2) 3))
(node (leaf 1) (leaf 2) 3)

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

(def root-storage (atom []))
(identity @root-storage)
(defn mmb-append-leaf [old-node new-leaf]
  (swap! root-storage conj (leaf (take-leaf)))
  )

(defn mmb-from-leafcount [leafcount]
  (reset! index -1)
  (reset! leaf-index -1)
  (swap! root-storage conj (leaf (take-index)))
  ;; (partition)
  @root-storage
  ;; (reduce (fn [root _]
  ;;           (mmb-append-leaf root (leaf (take-index))))
  ;;         (leaf (take-index))
  ;;         (range (dec leafcount)))
)

(filter (fn [[a b]] (= (mmr-leafcount a) (mmr-leafcount b))) (partition 2 1 [(leaf 1) (leaf 1) (mmr-from-leafcount 2) (mmr-from-leafcount 2)]))

(defn check-subsequency-and-recurse [accumulator remainder]
  (if (empty? remainder)
    accumulator
    (let [head (first remainder)]
     (if (= (first head) (second head))
       ;; merge-trees-and-fold-rest
       (concat (conj accumulator 1000) (map first (rest remainder)))
       (check-subsequency-and-recurse (conj accumulator (first head)) (rest remainder))
       ))
    )
  )

(check-subsequency-and-recurse [] (partition 2 1 [0] [1 2 3 3 4 5 6]))

(mmb-from-leafcount 1)

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

(defn find-subtree [root node-key & value?]
  (if (= ((if value? ::value ::index) root) node-key)
    root
    (if (has-children? root)
      (first
       (flatten
        (filter
         #(not (or (nil? %) (empty? %)))
         (map #(find-subtree % node-key value?) (children root))))))))

(mmr-from-leafcount 5)
(find-subtree (mmr-from-leafcount 5) 8)
(find-subtree (mmr-from-leafcount 9) "(0⋁1)⋁(2⋁3)" true)

(mmr-graph (mmr-from-leafcount 4))

(let [mmr (mmr-from-leafcount 4)
      graph (mmr-graph mmr)]
  (viz/view-graph (keys graph) graph
                  :node->descriptor (fn [n] {:label n})
                  ;; ::cluster->descriptor (fn [n] {::label n})
                  :node->cluster (fn [node-key] (mmr-depth (find-subtree mmr node-key join-labeling))))
  graph
  )

(comment
  (mmr-leafcount (::left (mmr-from-leafcount 14)))
  (mmr-leafcount (::right (mmr-from-leafcount 14)))

  (mmr-leafcount (::left (::left (mmr-from-leafcount 14))))
  (mmr-leafcount (::right (::left (mmr-from-leafcount 14)))))

(def example-mmr
  (do
    (reset! index -1)
    (node
     (node (leaf "a") (leaf "b") (take-index))
     (node (leaf "c") (leaf "d") (take-index))
     (take-index))))

(def extended-mmr (reduce (fn [root leaf-label] (mmr-append-leaf root (leaf leaf-label))) example-mmr ["e" "f" "g" "h"]))
