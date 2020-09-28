(ns core
  ;; (:require [rhizome.dot :as dot])
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

(defn log2 [num]
  (/ (Math/log num) (Math/log 2))
  (Math/floor)
  )

(int (Math/floor 2.5))

(rem)
(defn is-power-of-two [num] (range 0)
  (=
   (.pow (BigInteger. "2") (int (/ (Math/log num) (Math/log 2))))
   num)
  )

(is-power-of-two 5)
(is-power-of-two 8)

(Math/log 100)
(integer? (log2 4))

(defn mmr-append-leaf [old-node leaf]
  (if
      (is-power-of-two (mmr-leafcount old-node))
      ;; (or (= (mmr-leafcount old-node) 1)
      ;;     (= (mod (mmr-leafcount old-node) 2) 0)
      ;;     )
    (node old-node leaf)
    (node (:left old-node) (mmr-append-leaf (:right old-node) leaf))
    ))

(node {:value "a"} {:value "b"})
(node (leaf "a") (leaf "b"))

(mmr-append-leaf example-mmr (leaf "e"))

(def example-mmr
  (node
   (node (leaf "a") (leaf "b"))
   (node (leaf "c") (leaf "d")))
  )

(def extended-mmr (reduce (fn [root leaf-label] (mmr-append-leaf root (leaf leaf-label))) example-mmr ["e" "f" "g" "h"]))
(reduce (fn [root leaf-label] (mmr-append-leaf root (leaf leaf-label))) example-mmr ["e" "f" "g" "h"])

(defn mmr-from-leafcount [leafcount]
  (reduce (fn [root leaf-label]
            (mmr-append-leaf root (leaf leaf-label)))
          (leaf "a")
          ;; example-mmr
          (take (dec leafcount) (map (comp str char) (iterate inc (+ 98)))))
  )

(defn mmr-graph [root]
  (apply merge (flatten [ {(:value root) (map :value (children root))} (map mmr-graph (children root)) ]))
  ;; (merge {(:value root) (map :value (children root))} (mmr-graph (:left root)) (mmr-graph (:right root)))
  )

(mmr-leafcount (:left (mmr-from-leafcount 14)))
(mmr-leafcount (:right (mmr-from-leafcount 14)))

(mmr-leafcount (:left (:left (mmr-from-leafcount 14))))
(mmr-leafcount (:right (:left (mmr-from-leafcount 14))))

(def example-graph (mmr-graph (mmr-from-leafcount 4)))
(def example-graph-2 (mmr-append-leaf example-mmr (leaf "e")))
(def example-graph-3 (mmr-append-leaf example-graph-2 (leaf "f")))

(mmr-leafcount example-graph-2)

(def example-graph (mmr-graph (mmr-append-leaf example-mmr (leaf "e"))))

(def example-graph (mmr-graph (mmr-from-leafcount 7)))

(mmr-graph (mmr-from-leafcount 7))

;; (let [graph (mmr-graph (mmr-from-leafcount 14))]
;;   (dot/graph->dot (keys graph) graph
;;                   :node->descriptor (fn [n] {:label n})
;;                   :cluster->descriptor (fn [n] {:label n})
;;                   ;; :node->cluster identity
;;                   ;; :cluster->parent {"a" "b" "c" "d" "e" "f" "g" "h"}
;;                   ;; :cluster->ranks {"a" "b" "c" "d" "e" "f" "g" "h"}
;;                   :cluster->ranks {"d" "e"}
;;                   ;; :cluster->ranks {:a :b}
;;                   )
;;   )

(spit "test.dot" (let [graph (mmr-graph (mmr-from-leafcount 4))]
   (dot/graph->dot (keys graph) graph
                   :node->descriptor (fn [n] {:label n})
                   :cluster->descriptor (fn [n] {:label n})
                   ;; :node->clusters (fn [n] (when (number? n) (rem n 2)))
                   ;; :node->clusters (fn [n] (when (number? n) (rem n 2)))
                   ;; :node->clusters (fn [id] (if (filter #(= id %) ["a" "b" "c"]) ["aleph"]))
                   :node->clusters (fn [id] (if (filter #(= id %) ["a" "b" "c"]) ["aleph"]))
                   ;; :cluster->parent {"a" "b" "c" "d" "e" "f" "g" "h"}
                   ;; :cluster->ranks {"a" "b" "c" "d" "e" "f" "g" "h"}
                   :cluster->ranks {"a" "aleph"
                                    "b" "aleph"
                                    "c" "aleph"
                                    "d" "aleph"
                                    }
                   ;; :cluster->ranks {:a :b}
                   )))

(mmr-from-leafcount 2)
example-mmr
(mmr-from-leafcount 26)

(mmr-depth example-mmr)
(mmr-leafcount example-mmr)

(has-children? example-mmr)
(has-children? (:left example-mmr))
(has-children? (:left (:left example-mmr)))

(children example-mmr)
(identity example-mmr)

(def g
  {:a [:b :c]
   :b [:c]
   :c [:a :c]})

(spit "test-simple.dot"
      (dot/graph->dot (keys g) g
                      :cluster->descriptor (fn [n] {:label n})
                      :node->clusters (fn [n] [n])
                      :cluster->parent {:b :c, :a :c})
      )

(def g
  {:a [:b :c]
   :b [:c]
   :c [:a]})

(viz/view-graph (keys g) g
            :cluster->descriptor (fn [n] {:label n})
            :node->cluster (fn [n] (n {:a :c, :c :c}))
            :cluster->parent {:b :c, :a :c})

(let [graph (mmr-graph (mmr-from-leafcount 9))]
  (viz/view-graph (keys graph) graph
                  :node->descriptor (fn [n] {:label n})
                  ;; :cluster->descriptor (fn [n] {:label n})
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
                  ;; :node->cluster identity
                  ;; :cluster->parent {"a" "b" "c" "d" "e" "f" "g" "h"}
                  ;; :cluster->ranks {"a" "b" "c" "d" "e" "f" "g" "h"}
                  ;; :cluster->ranks {"d" "e"}
                  ;; :cluster->ranks {:a :b}
                  )
  )
