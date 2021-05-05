(ns core
  (:require [rhizome.viz :as viz]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.spec.test.alpha :as stest]
            )
  )

(def index (atom -1))

(s/def ::hash int?)

(s/def ::left ::hash)
(s/def ::right ::hash)
(s/def ::value int?)

(s/def ::leaf (s/keys :req [::value]))
(s/def ::parent (s/keys :req [::left ::right ::hash ::value]))
(s/def ::node (s/or :parent ::parent :leaf ::leaf))

(sgen/generate (s/gen ::hash))
(sgen/generate (s/gen ::left))
(sgen/generate (s/gen ::node))

(s/valid? ::node {::left 1 ::right 1 ::hash 1 ::value 1})

(s/def ::storage-map (s/map-of ::hash ::leaf))
(defonce storage (atom {}))
(s/valid? ::storage-map @storage)

(def root (atom nil))

(s/fdef reset-storage!
  :ret ::storage-map)
(defn reset-storage! [] (reset! storage {}))

(reset-storage!)
(def temp-storage @storage)

(defn hash-node [node]
  (if (has-children? node)
    (hash (str (::left node) (::right node)))
    (hash (::value node))
    )
  )

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

(defn node [left right]
  {::left left
   ::right right
   ::value (hash (str left right))})

(defn leaf [value]
  {::value value})

(defn children [node]
  (filter some?
          ((juxt ::left ::right) node)))

(defn has-children? [node]
  (not-empty (children node)))

(defn mmr-depth [node]
  (if (has-children? node)
    (+ 1
       (apply max (map mmr-depth (children node))))
    1
    ))

(defn storage-add! [node]
  (swap! storage assoc-in [(hash-node node)] node))

(defn is-root? [node])
(defn get-descendants [node]
  (if (has-children? node)
    (conj (children node) (map get-descendants (children node)))))

(get-descendants)
(identity @storage)

(storage-add! (leaf 2))
(storage-add! (node (leaf 1) (leaf 2)))
(node (leaf 1) (leaf 2))

(defn mmr-leafcount [node]
  (if (has-children? node)
    (apply + (map mmr-leafcount (children node)))
    1
    ))

(defn is-power-of-two [num]
  (=
   (.pow (BigInteger. "2") (int (/ (Math/log num) (Math/log 2))))
   num)
  )

(defn mmr-append-leaf [old-node leaf]
  (if
      (is-power-of-two (mmr-leafcount old-node))
    (node old-node leaf (take-index))
    (do (decrease-index) (node (::left old-node) (mmr-append-leaf (::right old-node) (assoc leaf ::value @index)) (take-index)))
    ))

(defn mmr-from-leafcount [leafcount]
  (reset! index -1)
  (reduce (fn [root _]
            (mmr-append-leaf root (leaf (take-index))))
          (leaf (take-index))
          (range (dec leafcount)))
  )

(defn mmr-graph [root]
  (apply merge (flatten [ {(::value root) (map ::value (children root))} (map mmr-graph (children root)) ]))
  )

(defn find-subtree [root node-key]
  (if (= (::value root) node-key)
    root
    (if (has-children? root)
      (first
       (flatten
        (filter
         #(not (or (nil? %) (empty? %)))
         (map #(find-subtree % node-key) (children root)))))
      )))

(find-subtree (mmr-from-leafcount 9) "(1⋁2)⋁(3⋁4)")

(let [
      mmr (mmr-from-leafcount 11)
      graph (mmr-graph mmr)
      ]
  (viz/view-graph (keys graph) graph
                  ::node->descriptor (fn [n] {::label n})
                  ;; ::cluster->descriptor (fn [n] {::label n})
                  ::node->cluster (fn [node-key] (mmr-depth (find-subtree mmr node-key)))
                  )
  ;; graph
  )

(comment
  (mmr-leafcount (::left (mmr-from-leafcount 14)))
  (mmr-leafcount (::right (mmr-from-leafcount 14)))

  (mmr-leafcount (::left (::left (mmr-from-leafcount 14))))
  (mmr-leafcount (::right (::left (mmr-from-leafcount 14))))
  )

(def example-mmr
  (do
    (reset! index -1)
    (node
     (node (leaf "a") (leaf "b") (take-index))
     (node (leaf "c") (leaf "d") (take-index))
     (take-index)))
  )

(def extended-mmr (reduce (fn [root leaf-label] (mmr-append-leaf root (leaf leaf-label))) example-mmr ["e" "f" "g" "h"]))
