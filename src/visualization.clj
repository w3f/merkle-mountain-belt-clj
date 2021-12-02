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
    ;; [range-node-edges range-nodes]
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
     ]
    )
  )

(storage/path (storage/name-index "p-1"))

(concat
 (apply concat
        (map #(list
               (list (:id %) (storage/node-name (storage/left-child (:index %))))
               (list (:id %) (storage/node-name (storage/right-child (:index %))))
               ) (storage/parents)))
 (drop-last
  (storage/range-node-edges
   (map storage/node-name (storage/parent-less-nodes)))))


(defn tangle-dot [graph]
  (#(apply tangle/graph->dot %) graph))

(defn tangle-direct [graph]
  (->
   graph
   tangle-dot
   (tangle/dot->image "png")
   javax.imageio.ImageIO/read
   ))

(->
 (graph "p-1")
 tangle-dot
 (tangle/dot->image "png")
 javax.imageio.ImageIO/read
 viz/view-image
 )

(defn tangle-direct-view [graph]
  (viz/view-image (tangle-direct graph)))

(defn tangle-direct-save [graph location]
  (viz/save-image (tangle-direct graph) location))

(tangle-direct-view (graph "p-1"))
(tangle-direct-save (graph "p-1") "jim")
(second (graph "p-1"))

(concat
 (apply concat
        (map #(list
               (list (:id %) (storage/node-name (storage/left-child (:index %))))
               (list (:id %) (storage/node-name (storage/right-child (:index %))))
               ) (storage/parents)))
 (first (storage/range-node-edges-reduced
   (map storage/node-name (storage/parent-less-nodes)))))

(comment
  (("p-1" 1) ("p-1" 2) ("p-2" 3) ("p-2" 4) ("p-3" "p-1") ("p-3" "p-2") ("p-4" 5) ("p-4" 6) ("p-5" 7) ("p-5" 8) ("p-6" "p-4") ("p-6" "p-5") ("p-7" 9) ("p-7" 10) ("p-8" "p-3") ("p-8" "p-6") ("p-9" 11) ("p-9" 12) ("p-10" "p-7") ("p-10" "p-9") ("p-11" 13) ("p-11" 14) ("p-12" 15) ("p-12" 16) ("p-13" "p-11") ("p-13" "p-12") ("p-14" 17) ("p-14" 18) ("p-15" "p-10") ("p-15" "p-13") ["p-8" "range-node-0"] [19 "range-node-0"] ["range-node-0" "range-node-1"] ["p-15" "range-node-1"] ["range-node-1" "range-node-2"] ["p-14" "range-node-2"]))

(->
 (concat
  (apply concat
         (map #(list
                (list (:id %) (storage/node-name (storage/left-child (:index %))))
                (list (:id %) (storage/node-name (storage/right-child (:index %))))
                ) (storage/parents)))
  (first (storage/range-node-edges-reduced
          (map storage/node-name (storage/parent-less-nodes)))))
 ;; decorate co-path edges
 (decorate-edges (storage/path (storage/name-index "p-1"))
                 {:style :dashed :color "blue"})
 )


(->
 (concat
  (apply concat
         (map #(list
                (list (:id %) (storage/node-name (storage/left-child (:index %))))
                (list (:id %) (storage/node-name (storage/right-child (:index %))))
                ) (storage/parents)))
  (first (storage/range-node-edges-reduced
          (map storage/node-name (storage/parent-less-nodes)))))
 ;; decorate co-path edges
 ;; (decorate-edges (storage/path (storage/name-index "p-1"))
 ;;                 {:style :dashed :color "blue"})
 )

(println
 (apply tangle/graph->dot
        [
         ;; nodes
         (reverse (into [] (flatten core/belted-edges)))

         ;; edges
         core/belted-edges


         {:node {:shape :oval}
          :node->id (fn [n] (:id n))
          :node->descriptor (fn [n] (when (map? n) n))
          }
         ]))

(graph "p-1")

((juxt
  tangle-direct-view
  ;; tangle-dot
  #(tangle-direct-save % (str "belted-edges-" @storage/leaf-count ".png")))
        [
         ;; nodes
         ;; (storage/node-name-maps (into [] (flatten core/belted-edges)))
         ;; (storage/node-maps-updated (into [] (into #{} (flatten (core/belted-nodes)))))
         ;; (core/graph-nodes)
         (map core/merge-positions (map #(update % :posx (fn [old] (* 1.8 old))) core/test-nodes-decorated))
         ;; (storage/node-maps (into [] (flatten (core/belted-edges))))
         ;; (into [] (flatten core/belted-edges))

         ;; edges
         (core/belted-edges)

         {:node {:shape :oval}
          :node->id (fn [n] (:id n))
          :node->descriptor (fn [n] (when (map? n) n))
          :graph {:rankdir :BT,
                  :label (str "n=" @storage/leaf-count),
                  :layout :neato}
          }
         ]
        )

(core/belted-edges)

(flatten core/belted-edges)
(nth @storage/storage-array 1536)
(storage/node-name 1536)

(storage/node-maps-updated (into [] (into #{} (flatten (core/belted-nodes)))))

(storage/node-maps (do (filter #(int? %) (flatten core/belted-edges))))
(map (juxt identity storage/node-name) (do (filter #(int? %) (flatten core/belted-edges))))
(storage/node-name-maps (flatten core/belted-edges))

(count (core/belted-edges))

(storage/node-maps '(1536))

(map (juxt #(storage/node-name (first %)) second) (storage/parent-less-nodes-sorted-height storage/parent-less-nodes-cache))
