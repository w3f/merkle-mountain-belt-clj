(ns visualization
  (:require [tangle.core :as tangle]
            [storage])
  )

;; prelim structure for visualization
(storage/parents)
(storage/node-name (storage/left-child 30))
(storage/left-child 30)

(storage/parent-index 2)
(map (storage/non-zero-entries))

(defn graph []
  [
   (map second (storage/non-zero-entries))
   (apply concat
          (map #(list
                 (list (second %) (storage/node-name (storage/left-child (first %))))
                 (list (second %) (storage/node-name (storage/right-child (first %))))
                 ) (storage/parents)))
   {:node {:shape :oval}
    :node->id (fn [n] (if (keyword? n) (name n) n))
    ;; :node->descriptor (fn [n] (when-not (keyword? n) n))
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
