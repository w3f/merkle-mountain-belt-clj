(ns primitives.visualization
  (:require
   [clojure.walk :as walk]
   [rhizome.viz :as viz]
   [tangle.core :as tangle]))

(defn decorate-nodes [nodes decorated-nodes decoration]
  (map
   #(if (contains? (into #{} decorated-nodes) (:id %))
      (merge decoration %)
      %) nodes))

(defn decorate-edge [edge decoration]
  (concat edge [decoration]))

(defn decorate-edges [edges decorated-edges decoration]
  (map #(if (and (contains? (into #{} decorated-edges) (first %)) (contains? (into #{} decorated-edges) (second %)))
          (decorate-edge % decoration) %)
       edges))

(defn tangle-dot [graph]
  (#(apply tangle/graph->dot %) graph))

(defn tangle-direct [graph]
  (->
   graph
   tangle-dot
   (tangle/dot->image "png")
   javax.imageio.ImageIO/read))

(defn tangle-direct-view [graph]
  (viz/view-image (tangle-direct graph)))

(defn tangle-direct-save [graph name]
  (spit (str "visualizations/" name ".svg") ((comp tangle/dot->svg tangle-dot) graph)))

(defn truncate-#set-display [data]
  (walk/postwalk
   #(if (and (contains? #{clojure.lang.PersistentHashSet
                          clojure.lang.PersistentTreeSet} (type %))
             (every? number? %))
      (if (< 1 (count %))
        (str "#{" (apply min %) ".." (apply max %) "}")
        (str %))
      %)
   ;; (sort-by #(apply min (:hash %)) data)
   data))
