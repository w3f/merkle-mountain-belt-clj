(ns visualization
  (:require
   clojure.walk
   [core]
   [linked-peaks]
   [primitives.storage]
   [rhizome.viz :as viz]
   [storage]
   [state]
   [tangle.core :as tangle])
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

;; TODO: decide at what layer to perform name parsing
(defn force-name-parsing [form]
  (clojure.walk/postwalk
   #(if (map? %) (core/parse-typed-name %) %)
   form)
  )

(force-name-parsing
 (primitives.storage/range-node-edges
  (map primitives.storage/node-name (primitives.storage/parent-less-nodes))))

(defn graph [starting-node]
  (let [[range-node-edges range-nodes] (force-name-parsing
                                        (primitives.storage/range-node-edges
                                         (map primitives.storage/node-name (primitives.storage/parent-less-nodes))))]
    ;; [range-node-edges range-nodes]
    [
     ;; nodes
     (->
      ;; nodes
      (concat
       ;; normal nodes
       (primitives.storage/non-zero-entries)
       ;; range nodes
       (map (fn [range-node] {:id range-node}) range-nodes))
      ;; decorate co-path nodes
      (decorate-nodes (force-name-parsing (primitives.storage/co-path (primitives.storage/name-index starting-node))) {:color "blue"})
      ;; decorate starting node
      (decorate-nodes #{starting-node} {:color "red"})
      )
     ;; edges
     (->
      (concat
       (apply concat
              (map #(list
                     (list (:id %) (primitives.storage/node-name (primitives.storage/left-child (:index %))))
                     (list (:id %) (primitives.storage/node-name (primitives.storage/right-child (:index %))))
                     ) (primitives.storage/parent-ids)))
       range-node-edges)
      ;; decorate co-path edges
      (decorate-edges (force-name-parsing (primitives.storage/path (primitives.storage/name-index starting-node)))
                      {:style :dashed :color "blue"})
      )
     {
      :node {:shape :oval}
      :node->id (fn [n] (:id n))
      :node->descriptor (fn [n] (when (map? n) n))
      }
     ]
    )
  )

(graph "p-1")
(comment (primitives.storage/path (primitives.storage/name-index "p-1")))

(concat
 (apply concat
        (map #(list
               (list (:id %) (primitives.storage/node-name (primitives.storage/left-child (:index %))))
               (list (:id %) (primitives.storage/node-name (primitives.storage/right-child (:index %))))
               ) (primitives.storage/parent-ids)))
 (drop-last
  (primitives.storage/range-node-edges
   (map primitives.storage/node-name (primitives.storage/parent-less-nodes)))))


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

;; DONE: fix trident
(let [n 1337]
  (->>
   (linked-peaks/graph n false)
   tangle-dot
   (tangle/dot->svg)
   (spit (str "ephemeral-nodes-" n ".svg"))))

(defn tangle-direct-view [graph]
  (viz/view-image (tangle-direct graph)))

;; TODO: fix trident in oneshot (incremental works)
(tangle-direct-view (linked-peaks/graph 1337 false))
(tangle-direct-view (linked-peaks/graph 1337 true))

(linked-peaks/toggle-debugging)
(linked-peaks/set-debugging-flags [:range-phantom])
(linked-peaks/play-algo-debug-last-step 6)
(@state/belt-nodes)
(@state/range-nodes)

(defn tangle-direct-save [graph location]
  (spit (str location ".svg") ((comp tangle/dot->svg tangle-dot) graph)))

(tangle-direct-view (graph "p-1"))
(tangle-direct-save (graph "p-1") "p-1")
(second (graph "p-1"))

(concat
 (apply concat
        (map #(list
               (list (:id %) (primitives.storage/node-name (primitives.storage/left-child (:index %))))
               (list (:id %) (primitives.storage/node-name (primitives.storage/right-child (:index %))))
               ) (primitives.storage/parent-ids)))
 (first (primitives.storage/range-node-edges-reduced
   (map primitives.storage/node-name (primitives.storage/parent-less-nodes)))))

(comment
  #_{:clj-kondo/ignore [:not-a-function]}
  (("p-1" 1) ("p-1" 2) ("p-2" 3) ("p-2" 4) ("p-3" "p-1") ("p-3" "p-2") ("p-4" 5) ("p-4" 6) ("p-5" 7) ("p-5" 8) ("p-6" "p-4") ("p-6" "p-5") ("p-7" 9) ("p-7" 10) ("p-8" "p-3") ("p-8" "p-6") ("p-9" 11) ("p-9" 12) ("p-10" "p-7") ("p-10" "p-9") ("p-11" 13) ("p-11" 14) ("p-12" 15) ("p-12" 16) ("p-13" "p-11") ("p-13" "p-12") ("p-14" 17) ("p-14" 18) ("p-15" "p-10") ("p-15" "p-13") ["p-8" "range-node-0"] [19 "range-node-0"] ["range-node-0" "range-node-1"] ["p-15" "range-node-1"] ["range-node-1" "range-node-2"] ["p-14" "range-node-2"]))

(->
 (concat
  (apply concat
         (map #(list
                (list (:id %) (primitives.storage/node-name (primitives.storage/left-child (:index %))))
                (list (:id %) (primitives.storage/node-name (primitives.storage/right-child (:index %))))
                ) (primitives.storage/parent-ids)))
  (first (primitives.storage/range-node-edges-reduced
          (map primitives.storage/node-name (primitives.storage/parent-less-nodes)))))
 ;; decorate co-path edges
 (decorate-edges (primitives.storage/path (primitives.storage/name-index "p-1"))
                 {:style :dashed :color "blue"})
 )


(->
 (concat
  (apply concat
         (map #(list
                (list (:id %) (primitives.storage/node-name (primitives.storage/left-child (:index %))))
                (list (:id %) (primitives.storage/node-name (primitives.storage/right-child (:index %))))
                ) (primitives.storage/parent-ids)))
  (first (primitives.storage/range-node-edges-reduced
          (map primitives.storage/node-name (primitives.storage/parent-less-nodes)))))
 ;; decorate co-path edges
 ;; (decorate-edges (primitives.storage/path (primitives.storage/name-index "p-1"))
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

(do
   (storage/run (inc 100))
   ((juxt
     tangle-direct-view
     ;; tangle-dot
     #(tangle-direct-save % (str "belted-edges-" @primitives.storage/leaf-count ".png")))
    [
     ;; nodes
     ;; (primitives.storage/node-name-maps (into [] (flatten core/belted-edges)))
     ;; (primitives.storage/node-maps-updated (into [] (into #{} (flatten (core/belted-nodes)))))
     ;; (core/graph-nodes)
     (map core/merge-positions (map #(update % :posx (fn [old] (* 1.8 old))) core/test-nodes-decorated))
     ;; (primitives.storage/node-maps (into [] (flatten (core/belted-edges))))
     ;; (into [] (flatten core/belted-edges))

     ;; edges
     (core/belted-edges)

     {:node {:shape :oval}
      :node->id (fn [n] (:id n))
      :node->descriptor (fn [n] (when (map? n) n))
      :graph {:rankdir :BT,
              :label (str "n=" @primitives.storage/leaf-count),
              :layout :neato}
      }
     ]))

(core/belted-edges)

(flatten (core/belted-edges))
(nth @primitives.storage/storage-array 1536)
(primitives.storage/node-name 1536)

(primitives.storage/node-maps-updated (into [] (into #{} (flatten (core/belted-nodes)))))

(primitives.storage/node-maps (filter #(int? %) (flatten (core/belted-edges))))
(map (juxt identity primitives.storage/node-name) (filter #(int? %) (flatten (core/belted-edges))))
(primitives.storage/node-name-maps (flatten (core/belted-edges)))

(count (core/belted-edges))

(primitives.storage/node-maps '(1536))

;; (map (juxt #(primitives.storage/node-name (first %)) second) (primitives.storage/parent-less-nodes-sorted-height @primitives.storage/parent-less-nodes-cache))
(map (juxt #(primitives.storage/node-name (first %)) second) (primitives.storage/parent-less-nodes-sorted-height (primitives.storage/parent-less-nodes)))
