(ns visualization-mmr
  (:require
   [core :refer [mmr-from-leafcount mmr-leafcount mmr-max-depth]]
   [primitives.core :refer [children has-children?]]
   [primitives.visualization :refer [truncate-#set-display]]
   [rhizome.viz :as viz]
   [tangle.core :as tangle]))

(defn mean-posx [node]
  (if (has-children? node)
    (/ (reduce + (map mean-posx (children node))) 2)
    (:core/value node)))

(defn graph [n]
  (let [mmr (mmr-from-leafcount n)
        nodes (atom nil)
        edges (atom nil)]
    (letfn [(add-nodes-edges [node]
              ;; (swap! nodes #(conj % (dissoc node ::left ::right)))
              (swap! nodes #(conj % {:index (:core/index node)
                                     :id (:core/index node)
                                     :label (:core/value node)
                                     :pos (str (float (mean-posx node))
                                               "," (mmr-max-depth node) "!")
                                     }))
              #_{:clj-kondo/ignore [:missing-else-branch]}
              (if (has-children? node)
                (do (swap! edges #(concat % [
                                             [(:core/index node) (:core/index (:core/left node))]
                                             [(:core/index node) (:core/index (:core/right node))]]))
                    (doall (map add-nodes-edges (children node))))))]
      (add-nodes-edges mmr)
      [
       ;; nodes
       @nodes

       ;; edges
       @edges

       ;; formatting options
       {:node {:shape :oval}
        :node->id (fn [n] (:id n))
        :node->descriptor (fn [n] (when (map? n) n))
        :graph {:rankdir :BT,
                :label (str "n=" (mmr-leafcount mmr)),
                :layout :neato}}
       ])
    ))

(->
 (graph 7)
 truncate-#set-display
 (#(apply tangle/graph->dot %))
 (tangle/dot->image "png")
 javax.imageio.ImageIO/read
 viz/view-image
 )
