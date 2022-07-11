(ns visualization-mmr
  (:require
   [core :refer [mmr-from-leafcount mmr-leafcount mmr-max-depth mmr-min-depth]]
   [primitives.core :refer [children has-children?]]
   [primitives.visualization :refer [tangle-direct-save truncate-#set-display]]
   [rhizome.viz :as viz]
   [tangle.core :as tangle]))

(defn mean-posx [node]
  (if (has-children? node)
    (/ (reduce + (map mean-posx (children node))) 2)
    (:core/value node)))

(defn get-peaks [root]
  (if (has-children? root)
    (cons (:core/left root) (get-peaks (:core/right root)))
    [root]))

(def temp-counter-index (atom 1000))

(defn l2r-mmr-from-r2l-mmr [mmr]
  (reset! temp-counter-index 1000)
  (reduce
   (fn [left right] (core/node left right (swap! temp-counter-index inc)))
   (get-peaks mmr)))

(defn graph [n l2r? unbagged?]
  (let [mmr (-> n
                mmr-from-leafcount
                (#(if l2r? (l2r-mmr-from-r2l-mmr %) %))
                (#(if l2r? (core/decorate-node-types-l2r %) (core/decorate-node-types %)))
                )
        nodes (atom nil)
        edges (atom nil)]
    (letfn [(add-nodes-edges [node]
              ;; (swap! nodes #(conj % (dissoc node ::left ::right)))
              (let [display? (not (and unbagged? (= :core/range (:core/type node))))]
                #_{:clj-kondo/ignore [:missing-else-branch]}
                (if display? (swap! nodes #(conj % {:index (:core/index node)
                                                    :id (:core/index node)
                                                    ;; :fillcolor ((:core/type node) {:core/node "grey"})
                                                    :color (or ((:core/type node) {:core/node "grey"
                                                                                   :core/leaf "lightblue"
                                                                                   :core/peak "red"})
                                                               "green")
                                                    :label (:core/value node)
                                                    :pos (str (float (mean-posx node))
                                                              "," (mmr-max-depth node) "!")
                                                    })))
                #_ {:clj-kondo/ignore [:missing-else-branch]}
                (if (has-children? node)
                  (do (if display? (swap! edges #(concat % [
                                                            [(:core/index node) (:core/index (:core/left node))]
                                                            [(:core/index node) (:core/index (:core/right node))]])))
                      (doall (map add-nodes-edges (children node)))))))]
      (add-nodes-edges mmr)
      [
       ;; nodes
       @nodes

       ;; edges
       @edges

       ;; formatting options
       {:node {:shape :egg}
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

(let [l2r? false
      unbagged? true]
  (map (fn [n] (tangle-direct-save (truncate-#set-display (graph n l2r? unbagged?))
                                  (str (if l2r? "f-" (if unbagged? "u-" "")) "mmr-n-" n)))
       (range 1 100)))
