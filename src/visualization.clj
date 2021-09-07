(ns visualization
  (:require [tangle.core :as tangle]
            [storage])
  )

;; prelim structure for visualization
(storage/parents)
(storage/node-name (storage/left-child 30))
(storage/left-child 30)

(defn graph []
  [
   (conj
    (map :name (storage/non-zero-entries))
    "RN")
   (concat
    (apply concat
           (map #(list
                  (list (:name %) (storage/node-name (storage/left-child (:index %))))
                  (list (:name %) (storage/node-name (storage/right-child (:index %))))
                  ) (storage/parents)))
    (map (fn [parent-less-node] ["RN" (storage/node-name (first parent-less-node))])
         (storage/parent-less-nodes)))
   {:node {:shape :oval}
    :node->id (fn [n] (if (keyword? n) (name n) n))
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

(tangle-direct-view (graph))
