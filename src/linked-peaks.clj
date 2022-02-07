(ns linked-peaks
  (:require [core]))

;; TODO: remove these entirely: they're only here for debugging now: only need hashes later
(defn internal-node [left height hash parent]
  {:left left
   :height height
   :hash hash
   :parent parent
   :type :internal})

(defn peak-node [left right height hash]
  {:left left
   :right right
   :height height
   :hash hash
   ;; peak nodes are instantiated without a parent
   :parent nil
   :type :peak})

(defn range-node [left right hash parent]
  {:left left
   :right right
   :hash hash
   :parent parent
   :type :range})

(defn belt-node [left right hash parent]
  {:left left
   :right right
   :hash hash
   :parent parent
   :type :belt})

(def lastP (atom nil))

;; (def R-count (atom 0))

(def mergeable-stack (atom []))

(def leaf-count (atom 0))

(def node-map (atom {}))
(def node-array (atom []))
(def belt-nodes (atom {}))
(def range-nodes (atom {}))
(def belt-children (atom {}))

(defn pop-mergeable-stack []
  (let [pop-item (last @mergeable-stack)]
    (swap! mergeable-stack (comp #(into [] %) drop-last))
    (get @node-map pop-item)))

(defn add-mergeable-stack [item]
  (swap! mergeable-stack #(assoc % (count %) (:hash item))))

(defn add-internal [item index]
  (let [array-len (count @node-array)
        ;; incidentally correct since index is calculated starting at 1 in lieu of 0
        zero-leaves (- index array-len)]
    (swap! node-array concat (repeat zero-leaves 0) (list item))))

(defn reset-all []
 (do
   (reset! node-map {})
   (reset! node-array [])
   (reset! mergeable-stack [])
   (reset! leaf-count 0)
   (reset! lastP nil)
   (reset! belt-nodes {})
   (reset! range-nodes {})
   ))

(:height (get @node-map @lastP))
(identity @lastP)

(defn hop-left [node & target-map]
  (:left (get (or (first target-map) @node-map) node)))

(defn hop-parent [node & target-map]
  (:parent (get (or (first target-map) @node-map) node)))

(def pointers (atom #{}))
;; could switch to object pointers to avoid :right values, but not convinced that the advantages outweigh the disadvantages:
;; +: don't need to update left pointers of right siblings
;; -: harder links to node-array
;; -: I suspect we need right pointers for peaks anyways to facilitate range node updates
(defn get-pointer []
  (let [pointer-upper-bound 9999
        pointer (first (drop-while
                        #(contains? @pointers %)
                        (repeatedly #(rand-int pointer-upper-bound))))]
    (swap! pointers #(conj % pointer))
    pointer)
  )
(reset! pointers #{})

(defn sanity-checks [Q-old]
  (if (= (:hash Q-old) (hop-left @lastP))
    (throw (Exception. ":left of lastP is outdated")))
  (if (= (:hash Q-old) (:left (get @node-map (:left (get @node-map @lastP)))))
    (throw (Exception. ":left of lastP's left is outdated")))
  (let [
        left-most-sibling-peak (last (take-while #(and (some? %) (not (contains? #{:internal :peak} (:type (get @node-map (hop-parent %)))))) (iterate hop-left @lastP)))
        correct-sibling-of-left-most (take-while #(and (some? %) (contains? #{:internal :peak} (:type (get @node-map %)))) (iterate hop-parent (hop-left left-most-sibling-peak)))
        ]
    (if (and (some? left-most-sibling-peak) (< 1 (count correct-sibling-of-left-most)))
      (throw (Exception. "should never get :left dissociated"))
      ;; #dbg
      ;; (swap! node-map #(assoc-in % [left-most-sibling-peak :left] (last correct-sibling-of-left-most)))
      )))

(comment
  (algo true))
(defn algo [oneshot-nesting?]
  (let [
        ;; let h be hash of new leaf
        ;; h (str @leaf-count "-hash")
        h #{@leaf-count}
        ;; pointer (get-pointer)
        ;; create object P, set P.hash<-h, set P.height<-0, set P.left<-lastP
        P (peak-node (:hash (get @node-map @lastP)) nil 0 h)
        ;; P (peak-node (:hash (get @node-map @lastP)) nil 0 h)
        ]
    (do
      ;; 1. Add step
      ;; store object P in peak map
      (swap! node-map #(assoc % h P))
      ;; (swap! node-map #(assoc % pointer P))
      ;; A[R*n+1]<-h
      (add-internal h (* 2 @leaf-count))

      ;; 2. Check mergeable
      ;; if lastP.height==0 then M.add(P)
      (if (and @lastP (= (:height (get @node-map @lastP)) 0))
        (add-mergeable-stack (get @node-map h)))

      ;; prelim: set right pointer of lastP to h
      ;; #dbg
      (if @lastP (swap! node-map #(assoc-in % [@lastP :right] h)))
      ;; 3. reset lastP
      (reset! lastP h)

      ;; 4. merge if mergeable
      ;; #dbg
      (if (not (zero? (count @mergeable-stack)))
        (do
          (let [Q (atom (pop-mergeable-stack))
                Q-old @Q
                ;; Q-old-hash (:hash Q-old)
                L (get @node-map (:left @Q))
                ]

            (swap! Q #(update % :height inc))
            (swap! Q #(assoc % :hash (apply sorted-set (concat (:hash L) (:hash Q-old)))))
            (swap! Q #(assoc % :left (:left L)))

            ;; Q and L (should) have a preexisting parent, either a range or a belt node
            (if (and (not oneshot-nesting?) (:parent Q-old))
              #dbg
              ;; just another check to ensure that we're merging
              (if (= (:parent Q-old)
                      (:parent L))
                ;; then
                (let [
                      ;; check where parent lives: should only exist in one of the maps
                      parent-contenders (filter some? (map #(get @% (:parent Q-old)) [node-map range-nodes belt-nodes]))
                      ]
                  ;; refactor here by splitting head of contenders from tail in let binding
                  (if (= 1 (count parent-contenders))
                    (if (not (contains? #{:internal :peak} (:type (first parent-contenders))))
                      (((:type (first parent-contenders)) {
                                                           :range (fn [] (do
                                                                          (swap! Q #(assoc % :parent (:parent (first parent-contenders))))
                                                                          (swap! range-nodes #(dissoc % (:hash (first parent-contenders))))
                                                                          ;; (throw (Exception. "unimplemented"))
                                                                          ))
                                                           :belt (fn [] (throw (Exception. "unimplemented")))
                                                           }))
                      (throw (Exception. "parent is an illegal: internal or peak"))
                      )
                    (throw (Exception. "multiple parent contenders - no bueno!"))
                    )
                  )
                ;; else
                (throw (Exception. (str "parents don't match @ leaf count " @leaf-count)))
                ;; introduce more complicated algorithm: if parents don't match, still valid if 
                ))
            ;; (if (= (:hash Q) #{8 9 10 11 12 13 14 15})
            (comment
              (if (= (:hash @Q) #{0 1 2 3 4 5 6 7})
               #dbg
               (if (:right Q-old)
                 (swap! node-map #(assoc-in % [(:right Q-old) :left] (:hash @Q))))))
            ;; add new leaf to node-map
            (swap! node-map #(assoc % (:hash @Q) @Q))
            ;; update :left pointer of Q-old's :right
            (if (:right Q-old)
              (swap! node-map #(assoc-in % [(:right Q-old) :left] (:hash @Q))))
            ;; update :right pointer of L's :left
            (if (:left L)
              (swap! node-map #(assoc-in % [(:left L) :right] (:hash @Q))))
            ;; update new parent-values
            ;; (swap! node-map #(assoc-in % [(:hash L) :parent] (:hash Q)))
            ;; (swap! node-map #(assoc-in % [Q-old-hash :parent] (:hash Q)))

            ;; update new parent-values, change type of children to internal (also removes :right key)
            ;; TODO: simply remove these nodes - they aren't needed beyond debugging and just clutter the interface
            (swap! node-map #(assoc % (:hash L) (internal-node (:left L) (:height L) (:hash L) (:hash @Q))))
            (swap! node-map #(assoc % (:hash Q-old) (internal-node (:left Q-old) (:height Q-old) (:hash Q-old) (:hash @Q))))
            ;; change type of children to internal
            ;; (swap! node-map #(assoc-in % [(:hash L) :type] :internal))
            ;; (swap! node-map #(assoc-in % [Q-old-hash :type] :internal))

            (add-internal (:hash @Q) (inc (* 2 @leaf-count)))
            ;; issue is that :left of Q can be outdated since may have had subsequent merge
            (if (= (:height @Q) (:height (get @node-map (:left @Q))))
              ;; #dbg
              (let [left's-parent (:parent (get @node-map (:left @Q)))]
                ;; TODO: debug why this broke when adding nil lefty and including last peak from prior range in current range
                (if (or (nil? left's-parent)
                        ;; DONE: This is now a meaningless test since node-map now only contains peaks and internal nodes
                        (contains? @range-nodes left's-parent)
)
                 (add-mergeable-stack @Q)
                 (throw (Exception. (str ":left should always be updated whenever we have a merge - can't have a non-ephemeral parent! leaf count " @leaf-count)))
                 ))
              )

            ;; if we've replaced the old lastP, should reset lastP to point to the new entry
            (if (= (:hash Q-old) @lastP)
              (reset! lastP (:hash @Q)))

            ;; TODO: the following has a smarter integration
            ;; (if (= 4 @leaf-count) (sanity-checks Q-old))

            (comment (if upgrade?))

            )
          )
        )
      (swap! leaf-count inc)


      ;; 5. TODO update range nodes

      (if oneshot-nesting? (oneshot-nesting true))
      ;; check (difference (S-n n) (S-n (dec n)))
      ;; recalculate only those members of S-n that are in the difference set from above

      ;; show results
      ;; (clojure.pprint/pprint [@node-map @node-array @mergeable-stack @lastP])
      ;; (clojure.pprint/pprint @node-map)
      ;; (clojure.pprint/pprint @node-map)
      ))
  )

;; test: bj always 1
(every? #(= "1" %)
        (map
         #(let [b-reverse (map str (reverse (storage/binary-repr-of-n (inc %))))
                j (storage/p-adic-order 2 (inc %))]
            (nth b-reverse j))
         (range 1500)))

(let [n 1339
      b (storage/binary-repr-of-n (inc n))
      ;; [b1 b0] (map #(Integer/parseInt %) (map str (drop (- (count b) 2) b)))
      j (storage/p-adic-order 2 (inc n))
      m1 (if (< 1 j) "new leaf joins last range"
             (get {0 "new leaf participates in merge"
                   1 "new leaf forms a range alone"} j))
      [bj+2 bj+1 bj] (map #(Integer/parseInt (str %)) (map #(nth (reverse b) %) [(+ j 2) (+ j 1) j]))
      m2 (if (= [bj+1 bj] [0 1])
           "M.M. not alone in range"
           (if (= [bj+2 bj+1 bj] [0 1 1])
             "M.M. alone in range"
             (if (= [bj+2 bj+1 bj] [1 1 1])
               "M.M. joins prev range"
               )))
      ;; {
      ;;  {
      ;;   1 "new leaf participates in merge"
      ;;   0 {:b1 {
      ;;           0 "new leaf joins last range"
      ;;           1 "new leaf formas a range alone"
      ;;           }}
      ;;   }
      ;;  :j {}
      ;;  }
      ;; j-map {0
      ;;        }
      ]
  [m1 m2]
  ;; [b1 b0 j]

  )

(some? (play-algo 1222 true))
(def algo-1222 (play-algo 1222 true))
(def algo-1223 (play-algo 1223 true))
(def algo-1277 (play-algo 1277 true))
(def algo-1278 (play-algo 1278 true))
(def algo-1279 (play-algo 1279 true))


(defn truncate-#set-display [data]
  (clojure.walk/postwalk
   #(if (and (contains? #{clojure.lang.PersistentHashSet
                          clojure.lang.PersistentTreeSet} (type %))
             (every? number? %))
      (if (< 2 (count %))
        (str "#{" (apply min %) ".." (apply max %) "}")
        (str %))
      %)
   (sort-by #(apply min (:hash %)) data))
  )

(defn display-type-filtered [data type]
  (truncate-#set-display
   (filter #(= type (:type %)) data)))

(truncate-#set-display (filter #(some? (:right %)) (vals (:node-map (play-algo 20 true)))))
(count (filter #(not= :internal (:type %)) (vals (:node-map (play-algo 1222 true)))))
(display-type-filtered (vals (:node-map (play-algo 1222 true))) :peak)
(display-type-filtered (vals (:node-map (play-algo 20 true))) :internal)

;; test: all peak nodes are connected and can be reached from one another
(let [nodes (:node-map algo-1222)
      peaks (filter #(not= :internal (:type %)) (vals nodes))
      left-most (filter #(nil? (:left %)) peaks)
      right-most (filter #(nil? (:right %)) peaks)
      chain-from-left (take-while some? (iterate #(get nodes (:right %)) (first left-most)))
      chain-from-right (take-while some? (iterate #(get nodes (:left %)) (first right-most)))
      ]
  {:only-peaks-and-all-peaks
   (and
    ;; check that only one node lacks a :left or a :right
    (every? #(= 1 (count %)) [left-most right-most])
    (not= left-most right-most)

    ;; check that only peaks are chained
    (every? #(= :peak (:type %)) chain-from-left)
    (every? #(= :peak (:type %)) chain-from-right)

    ;; check that every chain contains all peaks
    (every? #(= (count peaks) (count %)) [chain-from-left chain-from-right])
    )
   :left-most (map :hash left-most)
   :right-most (map :hash right-most)}
  )

(defn reset-atoms-from-cached [cached]
  (reset! node-map (:node-map cached))
  (reset! node-array (:node-array cached))
  (reset! mergeable-stack (:mergeable-stack cached))
  (reset! lastP (:lastP cached))
  (reset! leaf-count (:leaf-count cached))
  (reset! belt-nodes (:belt-nodes cached))
  (reset! range-nodes (:range-nodes cached))
  )

(defn current-atom-states []
  {
   :node-map @node-map
   :node-array @node-array
   :mergeable-stack @mergeable-stack
   :leaf-count @leaf-count
   :lastP @lastP
   :belt-nodes @belt-nodes
   :range-nodes @range-nodes
   })

(defn oneshot-nesting-from-cached [cached singleton-ranges?]
  (do (reset-atoms-from-cached cached)
      ;; (oneshot-nesting)
      (merge (current-atom-states) (oneshot-nesting singleton-ranges?))))

(defn oneshot-nesting-from-fresh [n singleton-ranges?]
  (do (play-algo n true)
      (merge (current-atom-states) (oneshot-nesting singleton-ranges?))))

;; test that caching vs fresh has same result
(let [fresh-1222 (oneshot-nesting-from-fresh 1222)
      cached-1222 (oneshot-nesting-from-cached algo-1222)]
  (= fresh-1222 cached-1222)
  )

(filter #(= :peak (:type (get (:node-map algo-1222) (nth (:node-array algo-1222) %))))
        (range (count (:node-array algo-1222))))
(comment (list 1533 1789 2301 2397 2413 2421 2429 2437 2441 2443))

;; NOTE: shifting storage left by 3 since skipping the constant offset from the beginning (always empty)
(map #(- % 3) (sort (storage/parent-less-nodes 1222)))

(defn oneshot-nesting
  "performs a oneshot nesting of ephemeral range and belt nodes. takes flag `singleton-ranges?` to specify whether singleton peaks should also have a range node above them"
  [singleton-ranges?]
  (let [
        ;; {:keys [node-map node-array]} (select-keys (play-algo @leaf-count upgrade?) [:node-map :node-array])
        ;; node-map (atom (:node-map algo-1222))
        ;; node-array (atom (:node-array algo-1222))
        ;; range-nodes (atom {})
        ;; belt-nodes (atom {})
        original-sorted-peaks (map #(get @node-map (nth @node-array (- (first %) 3))) (storage/parent-less-nodes-sorted-height (storage/parent-less-nodes @leaf-count)))
        ;; prepend nil as a peak to facilitate a linked list of peaks. TODO: abuse this as a pointer for the left-most peak ^^
        ;; DONE: add singleton-ranges? flag to later cases where relevant
        sorted-peaks (atom (if singleton-ranges? (cons (peak-node nil (:hash (first original-sorted-peaks)) nil nil) original-sorted-peaks) original-sorted-peaks))
        storage-maps {:peak node-map
                      :range range-nodes
                      :belt belt-nodes}]
    (reset! range-nodes {})
    (reset! belt-nodes {})
    (letfn [
            ;; takes type of child to find its storage map, and then updates its parent
            (update-parent [parent child]
              (swap! (get storage-maps (:type child)) (fn [storage-map] (assoc-in storage-map [(:hash child) :parent] (:hash parent)))))
            ]
      ;; #dbg
      (let [
            belt-children (doall (map (fn [belt-range-count]
                                        (reduce (fn [left-child right-child]
                                                  (let [left-most (:intruder left-child)
                                                        rn (range-node (:hash left-child) (:hash right-child)
                                                                       (clojure.set/union (if-not (and singleton-ranges? left-most) (:hash left-child)) (:hash right-child)) nil)]
                                                    (doall (map
                                                            (partial update-parent rn)
                                                            (if (and singleton-ranges? left-most) [right-child] [left-child right-child])))
                                                    (swap! range-nodes (fn [range-nodes] (assoc range-nodes (:hash rn) rn)))
                                                    rn
                                                    ))
                                                ;; returns all peaks that are in the given range. for every iteration, include the last node from the prior range, to make a linked list of all range nodes
                                                ;; DONE: tag the first node as NOT being in the same range
                                                (update (into [] (take (if singleton-ranges? (inc belt-range-count) belt-range-count)
                                                                       (first (swap-vals! sorted-peaks (fn [current] (drop belt-range-count current))))))
                                                        0 #(if singleton-ranges? (assoc % :intruder true) %))
                                                ))
                                      ;; returns number of nodes in each range
                                      (map count (core/belt-ranges @leaf-count))))
            ;; belt-children ()
            belts (doall
                   (reduce (fn [left-child right-child]
                             (let [bn (belt-node (:hash left-child) (:hash right-child)
                                                 (clojure.set/union (:hash left-child) (:hash right-child)) nil)]
                               (doall (map
                                       (partial update-parent bn)
                                       [left-child right-child]))
                               (swap! belt-nodes (fn [belt-nodes] (assoc belt-nodes (:hash bn) bn)))
                               bn
                               ))
                           belt-children
                           ))
            ]
        {:belt-children belt-children
         :range-nodes @range-nodes
         :belt-nodes @belt-nodes
         ;; :node-map node-map
         ;; :node-array node-array
         }))
    ))

(oneshot-nesting-from-fresh 9 false)
(def cached-oneshot-9 (oneshot-nesting-from-fresh 9 true))
(identity @range-nodes)
(identity @belt-nodes)
(:range-nodes cached-oneshot-9)
(:belt-nodes cached-oneshot-9)

(:belt-children (oneshot-nesting-from-fresh 9 true))
(=
 (map #(if (instance? clojure.lang.Atom %) @% %) (vals result-1222-cached))
 (map #(if (instance? clojure.lang.Atom %) @% %) (vals (oneshot-nesting-from-fresh 1222))))
(map some? (map (fn [val] (get @(:range-nodes result-1222) val))
                (map :hash (filter (fn [entry] (= :range (:type entry)))
                                   (map #(select-keys % [:type :hash]) (:belt-children result-1222))))))
;; -> can find all range nodes in collector

(map some? (map (fn [val] (get @(:node-map result-1222) val))
                (map :hash (filter (fn [entry] (= :range (:type entry)))
                                   ;; (map #(select-keys % [:type :hash]) (:belt-children result-1222) )))))
                                   (:belt-children result-1222)))))
;; -> can find first range node in collector

(map some? (map (fn [val] (:parent (get @(:range-nodes result-1222) val)))
                (map :hash (filter (fn [entry] (= :range (:type entry)))
                                   (map #(select-keys % [:type :hash]) (:belt-children result-1222))))))
;; ERGO -> the two last belt children don't have a daddy set, i.e. we're not updating this with final belt node? TODO: Investigate!!!

(count @(:range-nodes result-1222))
(count @(:belt-nodes result-1222))
;; ERGO -> it adds new range nodes, and the old ones don't attain parents!

(map some? (map (fn [val] (:parent (get @(:range-nodes result-1222) val)))
                (map :hash (filter (fn [entry] (= :range (:type entry)))
                                   (map #(select-keys % [:type :hash]) @(:range-nodes result-1222))))))
;; ERGO -> the two last belt children don't have a daddy set, i.e. we're not updating this with final belt node? TODO: Investigate!!!

(map some? (map (fn [val] (get @(:node-map result-1222) val))
      (map :hash (filter (fn [entry] (= :peak (:type entry))) (map #(select-keys % [:type :hash]) (:belt-children result-1222) )))))
;; -> peaks have parents set


(count @(:range-nodes result-1222))
(count @(:belt-nodes result-1222))
(get @(:range-nodes result-1222)
     (:hash (first (:belt-children result-1222))))

(map #(get (:node-map algo-1222)
           (nth (:node-array algo-1222) %))
     (map #(- % 3) (storage/parent-less-nodes 1222)))

(algo true)
(play-algo (last-algo-match) true)
(first-algo-mismatch)

(play-algo 300 false)

;; check that all peaks have correct children
(every? (fn [n]
          (eval `(and ~@((juxt
                          ;; number of leaves with this height
                          ;; (comp count second)
                          ;; does every leaf "hash" start with a 2^height
                          (comp (fn [list-first-indices] (every? #(= 0.0 %) (map #(mod % (Math/pow 2 n)) list-first-indices))) sort #(map first %) second)
                          ;; is every leaf "hash" a range from the first index until first index + 2^height?
                          (comp (fn [child-list] (every? #(= % (into #{} (range (first %) (+ (first %) (Math/pow 2 n))))) child-list)) second)
                          ;; identity
                          )
                         ;; group the peaks by hashset length
                         (nth (sort (group-by count (keys @node-map))) n)))))
        (range (count (core/S-n @leaf-count))))

(def algo-new-mismatch (play-algo (inc (last-algo-match)) true))
(def algo-old-mismatch (play-algo (inc (last-algo-match)) false))

(:mergeable-stack algo-new-mismatch)
(:mergeable-stack algo-old-mismatch)
(:lastP algo-new-mismatch)
(:lastP algo-old-mismatch)
(clojure.set/difference (into #{} (:node-map algo-new-mismatch))
                        (into #{} (:node-map algo-old-mismatch)))
(clojure.set/difference (into #{} (:node-map algo-old-mismatch))
                        (into #{} (:node-map algo-new-mismatch)))

(defn play-algo [n upgrade?]
  (do (reset-all)
      (doall (repeatedly n #(algo upgrade?)))
      ;; (println "-----------------")
      ;; (clojure.pprint/pprint @node-map)
      (current-atom-states)
      ))

;; test that that tree construction is correct
(let [n 1222
      nodes (play-algo n true)
      parent-less (filter #(= nil (:parent (val %))) (:node-map nodes))]
  (every? true?
          [
           (= (storage/S-n n) (reverse (sort (map (comp :height val) parent-less))))
           (= (storage/S-n n) (reverse (map (comp :height #(get @node-map %)) (take-while some? (iterate hop-left (:lastP nodes))))))
           (every? nil? (map #(:parent (get @node-map %)) (take-while #(some? (get @node-map %)) (iterate hop-left @lastP))))
           ]
          ))

(defn last-algo-match
  "plays algo while the upgrade and old algo still match"
  []
  (last (take-while
    #(let [
           non-upgrade (play-algo % false)
           upgrade (play-algo % true)
           ]
       (= non-upgrade upgrade))
    (range 300))))

(defn first-algo-mismatch
  "plays algo until first mismatch and returns the differences"
  []
  (let [first-mismatch (inc (last-algo-match))]
    (println "-----------------")
    (clojure.pprint/pprint
     {:first-mismatch first-mismatch
      :old (play-algo first-mismatch false)
      :new (play-algo first-mismatch true) })))

(do
  ;; (play-algo 10 false)
  (play-algo 0 false)
  (map (fn [[k v]] [k (:parent v)]) @node-map)
  (keys @node-map)
  )

(keys @node-map)
(get @node-map #{8 9})
