(ns visualization
  (:require [rhizome.viz :as viz]
            [tangle.core :as tangle]
            [storage])
  )

(defn decorate-nodes [nodes decorated-nodes decoration]
  (map
   #(if (contains? (into #{} decorated-nodes) (:id %))
      (merge decoration %)
      %) nodes)
  )

(defn decorate-edges [edges decorated-edges decoration]
  (map #(if (contains? (into #{} decorated-edges) (second %))
          (concat % [decoration]) %)
       edges)
    )

(defn graph [starting-node]
  (let [[range-node-edges range-nodes] (storage/range-node-edges
                                        (map storage/node-name (storage/parent-less-nodes)))]
    [
     ;; nodes
     (->
      (concat
       (storage/non-zero-entries)
       (map (fn [range-node] {:id range-node}) range-nodes))
      (decorate-nodes (storage/co-path (storage/name-index starting-node)) {:color "blue"})
      (decorate-nodes #{starting-node} {:color "red"})
      )
     ;; edges
     (->
      (concat
       (apply concat
              (map #(list
                     (list (:id %) (storage/node-name (storage/left-child (:index %))))
                     (list (:id %) (storage/node-name (storage/right-child (:index %))))
                     ) (storage/parents)))
       range-node-edges)
      (decorate-edges (storage/path (storage/name-index starting-node))
                      {:style :dashed :color "blue"})
      )
     {:node {:shape :oval}
      :node->id (fn [n] (:id n))
      :node->descriptor (fn [n] (when (map? n) n))
      }
     ])
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
