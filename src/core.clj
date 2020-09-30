(ns core
  (:require [rhizome.viz :as viz])
  )

(def index (atom 0))

(defn take-index []
  (swap! index inc))

(defn decrease-index []
  (swap! index dec))

(defn parentheses-maybe [string]
  (if (nil? (re-find #"⋁" string))
    string
    (str "(" string ")")))

(defn node [left right value]
  {:left left
   :right right
   :value value})

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
    (node old-node leaf (take-index))
    (do (decrease-index) (node (:left old-node) (mmr-append-leaf (:right old-node) (assoc leaf :value @index)) (take-index)))
    ))

(defn mmr-from-leafcount [leafcount]
  (reset! index 0)
  (reduce (fn [root _]
            (mmr-append-leaf root (leaf (take-index))))
          (leaf (take-index))
          (range (dec leafcount)))
  )

(defn mmr-graph [root]
  (apply merge (flatten [ {(:value root) (map :value (children root))} (map mmr-graph (children root)) ]))
  )

(defn find-subtree [root node-key]
  (if (= (:value root) node-key)
    root
    (if (has-children? root)
      (first
       (flatten
        (filter
         #(not (or (nil? %) (empty? %)))
         (map #(find-subtree % node-key) (children root)))))
      )))

(find-subtree (mmr-from-leafcount 9) "abcd")

(let [
      mmr (mmr-from-leafcount 9)
      graph (mmr-graph mmr)
      ]
  (viz/view-graph (keys graph) graph
                  :node->descriptor (fn [n] {:label n})
                  ;; :cluster->descriptor (fn [n] {:label n})
                  :node->cluster (fn [node-key] (mmr-depth (find-subtree mmr node-key)))
                  )
  )

(comment
  (mmr-leafcount (:left (mmr-from-leafcount 14)))
  (mmr-leafcount (:right (mmr-from-leafcount 14)))

  (mmr-leafcount (:left (:left (mmr-from-leafcount 14))))
  (mmr-leafcount (:right (:left (mmr-from-leafcount 14))))
  )

(def example-mmr
  (do
    (reset! index 0)
    (node
     (node (leaf "a") (leaf "b") (take-index))
     (node (leaf "c") (leaf "d") (take-index))
     (take-index)))
  )

(def extended-mmr (reduce (fn [root leaf-label] (mmr-append-leaf root (leaf leaf-label))) example-mmr ["e" "f" "g" "h"]))
