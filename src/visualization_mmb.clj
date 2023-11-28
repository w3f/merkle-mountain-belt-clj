(ns visualization-mmb
  (:require
   clojure.set
   clojure.walk
   [core]
   [linked-peaks]
   [primitives.storage :refer [children]]
   [primitives.visualization :refer [decorate-edges decorate-nodes style
                                     tangle-direct-dot tangle-direct-save
                                     tangle-direct-view tangle-dot
                                     truncate-#set-display]]
   [rhizome.viz :as viz]
   [state]
   [storage]
   [tangle.core :as tangle]))

;; TODO: decide at what layer to perform name parsing
(defn force-name-parsing [form]
  (clojure.walk/postwalk
   #(if (map? %) (core/parse-typed-name %) %)
   form))

(defn position [storage-entry]
  (if (integer? (:id storage-entry))
    [(:id storage-entry) 0]
    (reduce
     (fn [[x0 y0] [x1 y1]] [(float (/ (+ x0 x1) 2)) (inc (/ (+ y0 y1) 2))])
     (map (comp position #(nth (primitives.storage/node-maps @primitives.storage/storage-array) %)) (children (:index storage-entry))))))

;; TODO: seems that I added this to replace primitives.storage/node-name
(defn name [storage-entry]
  (if (integer? (:id storage-entry))
    (:id storage-entry)
    (let [[left right] (map
                        (comp name #(nth (primitives.storage/node-maps @primitives.storage/storage-array) %))
                        (children (:index storage-entry)))]
      (if (integer? left)
        #{left right}
        (clojure.set/union left right)))))

(defn decorate-name [node]
  (assoc node :id (let [name (name node)]
                    (if (and (contains? #{clojure.lang.PersistentHashSet
                                          clojure.lang.PersistentTreeSet} (type name))
                             (every? number? name))
                      (if (< 1 (count name))
                        (str "#{" (apply min name) ".." (apply max name) "}")
                        (str name))
                      name))))

(defn decorate-position [node]
  (assoc node :pos ((fn [[x y]] (str x "," y "!")) (position node))))

(force-name-parsing
 (primitives.storage/range-node-edges
  (map primitives.storage/node-name (primitives.storage/parent-less-nodes))))

(defn graph [starting-node bagged?]
  (let [[range-node-edges range-nodes] (force-name-parsing
                                        (primitives.storage/range-node-edges
                                         (map primitives.storage/node-name (primitives.storage/parent-less-nodes))))]
    ;; [range-node-edges range-nodes]
    [;; nodes
     (->
      ;; nodes
      (concat
       ;; normal nodes
       (map (comp decorate-name decorate-position) (primitives.storage/non-zero-entries))
       ;; range nodes
       (if bagged? (map (fn [range-node] {:id range-node}) range-nodes) '()))
      ;; decorate co-path nodes
      ;; (decorate-nodes (force-name-parsing (primitives.storage/co-path (primitives.storage/name-index starting-node))) {:color "blue"})
      ;; decorate starting node
      ;; (decorate-nodes #{starting-node} {:color "red"})
      )

     ;; edges
     (->
      (concat
       (apply concat
              (map #(list
                     (list (:id (decorate-name %)) (:id (decorate-name (nth (primitives.storage/node-maps @primitives.storage/storage-array) (primitives.storage/left-child (:index %))))))
                     (list (:id (decorate-name %)) (:id (decorate-name (nth (primitives.storage/node-maps @primitives.storage/storage-array) (primitives.storage/right-child (:index %))))))) (primitives.storage/parent-ids)))
       ;; (if bagged? range-node-edges '())
       )

      ;; decorate co-path edges
      ;; (decorate-edges (force-name-parsing (primitives.storage/path (primitives.storage/name-index starting-node)))
      ;;                 {:style :dashed :color "blue"})
      )

     ;; formatting options
     {:node {:shape :egg}
      :node->id (fn [n] (:id n))
      :node->descriptor (fn [n] (when (map? n) n))
      :graph {:rankdir :BT,
              :label (str "n=" @primitives.storage/leaf-count),
              :layout :neato}
      }
     ;; {}
     ]))

(defn construct-graph [n starting-node bagged?]
  (storage/run n)
  (graph starting-node bagged?))

(graph "p-1" true)
(comment (primitives.storage/path (primitives.storage/name-index "p-1")))

(concat
 (apply concat
        (map #(list
               (list (:id %) (primitives.storage/node-name (primitives.storage/left-child (:index %))))
               (list (:id %) (primitives.storage/node-name (primitives.storage/right-child (:index %))))) (primitives.storage/parent-ids)))
 (drop-last
  (primitives.storage/range-node-edges
   (map primitives.storage/node-name (primitives.storage/parent-less-nodes)))))

(->
 (construct-graph 9 1 false)
 tangle-dot
 (tangle/dot->image "png")
 javax.imageio.ImageIO/read
 viz/view-image)

(let [n 1337]
  (->>
   (linked-peaks/graph n nil false true true true)
   tangle-dot
   (tangle/dot->svg)
   (spit (str "visualizations/ephemeral-nodes-" n ".svg"))))

(let [n 17]
  (->
   (linked-peaks/graph n nil false true true true)
   tangle-dot
   (tangle/dot->image "png")
   javax.imageio.ImageIO/read
   viz/view-image))

(let [belting? true
      hide-helper-nodes? true
      fixed-pos? true
      leaf-to-prove nil]
  (map (fn [n] (->
               (linked-peaks/graph n leaf-to-prove false belting? hide-helper-nodes? fixed-pos?)
               (tangle-direct-save (str (if hide-helper-nodes? "" "verbose-") (if belting? "" "u-") "mmb-n-" n))
               ))
       (range 1 64)))

(linked-peaks/toggle-debugging)
(linked-peaks/set-debugging-flags [:range-phantom])
(linked-peaks/play-algo-debug-last-step 6)
@state/belt-nodes
@state/range-nodes

(tangle-direct-view (construct-graph 11 nil "p-1"))
(tangle-direct-save (graph nil "p-1") "p-1")
(second (graph nil "p-1"))

(concat
 (apply concat
        (map #(list
               (list (:id %) (primitives.storage/node-name (primitives.storage/left-child (:index %))))
               (list (:id %) (primitives.storage/node-name (primitives.storage/right-child (:index %))))) (primitives.storage/parent-ids)))
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
                (list (:id %) (primitives.storage/node-name (primitives.storage/right-child (:index %))))) (primitives.storage/parent-ids)))
  (first (primitives.storage/range-node-edges-reduced
          (map primitives.storage/node-name (primitives.storage/parent-less-nodes)))))
 ;; decorate co-path edges
 (decorate-edges (primitives.storage/path (primitives.storage/name-index "p-1"))
                 {:style :dashed :color "blue"}))

(->
 (concat
  (apply concat
         (map #(list
                (list (:id %) (primitives.storage/node-name (primitives.storage/left-child (:index %))))
                (list (:id %) (primitives.storage/node-name (primitives.storage/right-child (:index %))))) (primitives.storage/parent-ids)))
  (first (primitives.storage/range-node-edges-reduced
          (map primitives.storage/node-name (primitives.storage/parent-less-nodes)))))
 ;; decorate co-path edges
 ;; (decorate-edges (primitives.storage/path (primitives.storage/name-index "p-1"))
 ;;                 {:style :dashed :color "blue"})
 )

(println
 (apply tangle/graph->dot
        [;; nodes
         (reverse (into [] (flatten (core/belted-edges))))

         ;; edges
         core/belted-edges

         {:node {:shape :oval}
          :node->id (fn [n] (:id n))
          :node->descriptor (fn [n] (when (map? n) n))}]))

;; this is completely broken
(do
  (storage/run (inc 100))
  ((juxt
    tangle-direct-view
    ;; tangle-dot
    #(tangle-direct-save % (str "belted-edges-" @primitives.storage/leaf-count)))
   [;; nodes
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
             :layout :neato}}]))

(map core/merge-positions (map #(update % :posx (fn [old] (* 1.8 old))) core/test-nodes-decorated))

(primitives.storage/node-maps-updated (into [] (into #{} (flatten (core/belted-nodes)))))

(primitives.storage/node-maps (filter #(int? %) (flatten (core/belted-edges))))
(map (juxt identity primitives.storage/node-name) (filter #(int? %) (flatten (core/belted-edges))))
(primitives.storage/node-name-maps (flatten (core/belted-edges)))

(count (core/belted-edges))

(primitives.storage/node-maps '(1536))

;; (map (juxt #(primitives.storage/node-name (first %)) second) (primitives.storage/parent-less-nodes-sorted-height @primitives.storage/parent-less-nodes-cache))
(map (juxt #(primitives.storage/node-name (first %)) second) (primitives.storage/parent-less-nodes-sorted-height (primitives.storage/parent-less-nodes)))
