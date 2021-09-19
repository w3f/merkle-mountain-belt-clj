(ns visualization
  (:require [rhizome.viz :as viz]
            [tangle.core :as tangle]
            [storage])
  )

(defn decorate-nodes [nodes decorated-nodes decoration]
  (map
   #(if (contains? (into #{} decorated-nodes) %)
      (assoc decoration
             :id %
             ;; :label %
             )
      %) nodes)
  )

(defn decorate-edges [edges decorated-edges decoration]
  (map #(if (contains? (into #{} decorated-edges) (second %))
          (concat % [decoration]) %)
       edges)
    )

(defn graph [starting-node]
  [
   ;; nodes
   (->
    (conj
      (map :name (storage/non-zero-entries))
      "RN")
    (decorate-nodes (storage/co-path (storage/name-index starting-node)) {:color "blue"})
    (decorate-nodes #{starting-node} {:color "red"})
    )
   ;; edges
   (->
    (concat
     (apply concat
            (map #(list
                   (list (:name %) (storage/node-name (storage/left-child (:index %))))
                   (list (:name %) (storage/node-name (storage/right-child (:index %))))
                   ) (storage/parents)))
     (map (fn [parent-less-node] ["RN" (storage/node-name (first parent-less-node))])
          (storage/parent-less-nodes)))
    (decorate-edges (storage/path (storage/name-index starting-node))
                    {:style :dashed :color "blue"})
    )
   {:node {:shape :oval}
    :node->id (fn [n] (if (keyword? n)
                       (name n)
                       (if (map? n) (:id n) n)))
    :node->descriptor (fn [n] (when (map? n) n))
    }
   ]
  )

(defn tangle-direct-view [graph]
  (->
   graph
   (#(apply tangle/graph->dot %))
   (tangle/dot->image "png")
   javax.imageio.ImageIO/read
   viz/view-image
   ))

(tangle-direct-view (graph "p-5"))
