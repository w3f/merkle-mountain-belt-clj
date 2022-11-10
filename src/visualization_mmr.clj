(ns visualization-mmr
  (:require
   [clojure.set :as set]
   [clojure.test :as test]
   [core :refer [copath-nodes decorate-node is-peak? mmr-from-leafcount
                 mmr-leafcount mmr-max-depth path]]
   [primitives.core :refer [children has-children?]]
   [primitives.visualization :refer [style tangle-direct-dot tangle-direct-save truncate-#set-display]]
   [rhizome.viz :as viz]
   [tangle.core :as tangle]))

(defn mean-posx [node]
  (if (has-children? node)
    (/ (reduce + (map mean-posx (children node))) 2)
    (:core/value node)))

(defn get-peaks [root]
  (if (and (has-children? root) (not (is-peak? root)))
    (cons (:core/left root) (get-peaks (:core/right root)))
    [root]))

(defn get-ranges [root]
  (if (and (has-children? root) (not (is-peak? root)))
    (cons root (get-ranges (:core/right root)))
    []))

(test/deftest peaks-and-ranges-distinct
  (clojure.test/is (= #{#{}}
                      (into #{} (map (fn [n] (apply set/intersection (map #(into #{} %) ((juxt get-peaks get-ranges) (mmr-from-leafcount n)))))
                                     (range 100)))))
  )

(test/run-test peaks-and-ranges-distinct)
;; Ran 1 tests containing 1 assertions.
;; 0 failures, 0 errors.

(defn l2r-mmr-from-r2l-mmr [mmr]
  (let [root-indices (atom (reverse (map :core/index (get-ranges mmr))))]
    (reduce
     (fn [left right] (core/node left right ((fn [] (let [x (first @root-indices)]
                                                    (swap! root-indices #(rest %))
                                                    x)))))
     (get-peaks mmr))))

(defn graph [n leaf-to-prove l2r? unbagged?]
  (let [mmr (-> n
                mmr-from-leafcount
                (#(if l2r? (l2r-mmr-from-r2l-mmr %) %))
                (#(if l2r? (core/decorate-node-types-l2r %) (core/decorate-node-types %)))
                )
        nodes (atom nil)
        edges (atom nil)
        path (if leaf-to-prove (path mmr leaf-to-prove) nil)
        copath-indices (if leaf-to-prove
                         (into #{} (map :core/index (copath-nodes mmr path)))
                         nil)]
    (letfn [(add-nodes-edges [node]
              ;; (swap! nodes #(conj % (dissoc node ::left ::right)))
              (let [display? (not (and unbagged? (= :core/range (:core/type node))))]
                #_{:clj-kondo/ignore [:missing-else-branch]}
                (if display? (swap! nodes #(conj % {:index (:core/index node)
                                                    :id (:core/index node)
                                                    ;; :fillcolor ((:core/type node) {:core/node "grey"})
                                                    :color (if leaf-to-prove
                                                             (:default style)
                                                             (or ((:core/type node) {:core/node (:default style)
                                                                                     :core/leaf (:leaf style)
                                                                                     :core/peak (:peak style)})
                                                                 (:range style)))
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
       (if leaf-to-prove
         (map
          ;; decorate nodes on path
          #(decorate-node % #{(:core/index (get-in mmr path))} {:color (:path style)})
          ;; decorate nodes on copath
          (map #(decorate-node % copath-indices {:color (:copath style)}) @nodes))
         @nodes)

       ;; edges
       @edges

       ;; formatting options
       {:node {:shape :egg}
        :node->id (fn [n] (:id n))
        :node->descriptor (fn [n] (when (map? n) n))
        :graph {:rankdir :BT,
                :label (str "n=" (mmr-leafcount mmr)),
                ;; :bgcolor "transparent",
                :bgcolor (:background style),
                :fontcolor (:foreground style),
                :fontname (:font style),
                :layout :neato}}
       ])
    ))

(->
 (graph 11 nil false false)
 truncate-#set-display
 (#(apply tangle/graph->dot %))
 (tangle/dot->image "png")
 javax.imageio.ImageIO/read
 viz/view-image
 )

(let [l2r? true
      unbagged? false]
  (map (fn [n] (-> n
                   (graph nil l2r? unbagged?)
                   truncate-#set-display
                   (tangle-direct-save (str (if l2r? "f-" (if unbagged? "u-" "")) "mmr-n-" n))))
       (range 1 100)))
