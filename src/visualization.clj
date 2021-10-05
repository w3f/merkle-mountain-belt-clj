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
  (map #(if (and (contains? (into #{} decorated-edges) (first %)) (contains? (into #{} decorated-edges) (second %)))
          (concat % [decoration]) %)
       edges)
    )

(defn graph [starting-node]
  (let [[range-node-edges range-nodes] (storage/range-node-edges
                                        (map storage/node-name (storage/parent-less-nodes)))]
    [
     ;; nodes
     (->
      ;; nodes
      (concat
       ;; normal nodes
       (storage/non-zero-entries)
       ;; range nodes
       (map (fn [range-node] {:id range-node}) range-nodes))
      ;; decorate co-path nodes
      (decorate-nodes (storage/co-path (storage/name-index starting-node)) {:color "blue"})
      ;; decorate starting node
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
      ;; decorate co-path edges
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
