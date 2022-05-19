(ns primitives.storage)

(defn highest-exponent-st-dividing [p n]
  ;; TODO: improve this algorithm - it's inefficient as fuck!
  (last
   (filter #(= 0.0
               (mod (Math/abs n) (Math/pow p %)))
           (range 0 (Math/abs n)))))

(let [log (/ (Math/log 2048) (Math/log 2))] (= 0.0 (- log (int log))))

(defn p-adic-order [p n]
  (if (= 0 n)
    ##Inf
    (highest-exponent-st-dividing p n)
    ))

(comment
  (map #(p-adic-order 2 %) (range 1 100)))

(defn left-child [parent]
  (- parent (* 3 (int (Math/pow 2 (- (p-adic-order 2 parent) 1))))))

(defn right-child [parent]
  (- parent (int (Math/pow 2 (- (p-adic-order 2 parent) 1)))))

(defn childedness [node]
  (get {3 :left
        1 :right} (int (mod (/ node (Math/pow 2 (p-adic-order 2 node))) 4))))

(defn parent-index [child-index]
  (+ child-index (mod child-index (int (Math/pow 2 (+ (primitives.storage/p-adic-order 2 child-index) 2))))))

;; defined using `peak-positions-final`
(defn parent-less-nodes-internal
  "get peaks for current leaf-count or `n`"
  [n]
  (let [array-size (+ 2 (* 2 n))]
    (reduce #(let [adic (int (Math/pow 2 %2))
                        prepreindex (- array-size (mod array-size adic))
                        ;; TODO: refactor since this only supports two collisions
                        preindex (if (let [log (/ (Math/log prepreindex) (Math/log 2))]
                                       (or
                                        (= 0.0 (- log (int log)))
                                        (some (fn [existing-index] (= existing-index prepreindex)) (reverse %1))
                                        ))
                                   (- prepreindex adic)
                                   prepreindex)
                        index (if (let [log (/ (Math/log preindex) (Math/log 2))]
                                    (or
                                     (= 0.0 (- log (int log)))
                                     (some (fn [existing-index] (= existing-index preindex)) (reverse %1))
                                     ))
                                (- preindex adic)
                                preindex)
                        ]
                    (conj %1 index)
                    )
                 []
                 (primitives.core/S-n n))
    ))

(defonce leaf-count (atom 0))
(defonce node-count (atom 0))

(defn parent-less-nodes
  "get peaks for current leaf-count or `n`"
  ; TODO: probably using the leaf-count is bad
  ([] (parent-less-nodes @leaf-count))
  ([n]
   (into #{} (parent-less-nodes-internal n))))

;; hacky calculation of parent: if number, take position in (max (position-parent-less-nodes))
(defn position-parentless-nodes [node]
  (first (filter #(= node (nth (into [] (parent-less-nodes)) %)) (range (count (parent-less-nodes))))))

(defn node-height-literal
  "takes the node index `n` and returns the node's height"
  [n]
  (let [child-iterator (iterate left-child n)]
    (count (take-while #(not= (nth child-iterator %)
                              (nth child-iterator (inc %)))
                       (range 3000))))
  )

(defn parent-less-nodes-sorted-height
  "sorts nodes by height (inverse), with tie-breaker being the node index"
  [nodes]
  (sort #(compare (nth %2 1) (nth %1 1))
        (map (juxt identity node-height-literal) (sort nodes))))
