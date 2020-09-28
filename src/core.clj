(ns core
  (:require [rhizome.viz :as viz])
  )

(defn node [left right]
  {:left left
   :right right
   :value (str (:value left) (:value right))})

(defn leaf [value]
  {:value value})

(defn children [node]
  (filter some?
          ((juxt :left :right) node)))

(defn has-children? [node]
  (not-empty (children node)))

(defn mmr-depth [node]
  (if (has-children? node)
    (+ 1
       (apply max (map mmr-depth (children node))))
    1
    ))

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
    (node old-node leaf)
    (node (:left old-node) (mmr-append-leaf (:right old-node) leaf))
    ))

(defn mmr-from-leafcount [leafcount]
  (reduce (fn [root leaf-label]
            (mmr-append-leaf root (leaf leaf-label)))
          (leaf "a")
          (take (dec leafcount) (map (comp str char) (iterate inc (+ 98)))))
  )

(defn mmr-graph [root]
  (apply merge (flatten [ {(:value root) (map :value (children root))} (map mmr-graph (children root)) ]))
  )

(let [graph (mmr-graph (mmr-from-leafcount 9))]
  (viz/view-graph (keys graph) graph
                  :node->descriptor (fn [n] {:label n})
                  :cluster->descriptor (fn [n] {:label n})
                  :node->cluster (fn [n] (get
                                         {"a" "aleph",
                                          "b" "aleph",
                                          "c" "aleph",
                                          "d" "aleph",
                                          "e" "aleph",
                                          "f" "aleph",
                                          "g" "aleph",
                                          "h" "aleph",
                                          "i" "aleph",
                                          } n))
                  )
  )

(comment
  (mmr-leafcount (:left (mmr-from-leafcount 14)))
  (mmr-leafcount (:right (mmr-from-leafcount 14)))

  (mmr-leafcount (:left (:left (mmr-from-leafcount 14))))
  (mmr-leafcount (:right (:left (mmr-from-leafcount 14))))
  )

(def example-mmr
  (node
   (node (leaf "a") (leaf "b"))
   (node (leaf "c") (leaf "d")))
  )

(def extended-mmr (reduce (fn [root leaf-label] (mmr-append-leaf root (leaf leaf-label))) example-mmr ["e" "f" "g" "h"]))
