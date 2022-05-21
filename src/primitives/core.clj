(ns primitives.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            ))

(defn binary-repr-of-n [n]
  (Integer/toBinaryString n))

(defn bits-of-n [n]
  (map (comp #(Integer. %) str) (binary-repr-of-n n)))

(defn bits-of-inc-n [n]
  (bits-of-n (inc n)))

(defn binary-repr-of-n [n]
  (Integer/toBinaryString n))

;; test for n=1221
(comment
  (S-n 1221)
  (binary-repr-of-n 1222))

(defn S-n-deprecated
  "list of mountain heights for leaf-count `n`"
  [n]
  (let [bits (bits-of-inc-n n)
        reversed-bits (reverse bits)]
    (reverse (map
              #(+ % (nth reversed-bits %))
              (range (dec (count bits)))))))

(defn S-n
  "list of mountain heights for leaf-count `n`"
  [n]
  (let [bits (map (comp #(Integer. %) str) (binary-repr-of-n (inc n)))
        reversed-bits (reverse bits)]
    (reverse (map
              #(+ % (nth reversed-bits %))
              (range (dec (count bits)))))))

(every? #(= (S-n %) (S-n-deprecated %)) (range 20000))

(last (S-n 1222))

;; 1222 (9 8 8 7 5 4 3 3 2 1)
(S-n 19)
(S-n 1222)
(S-n 1223)

(S-n 3)
;; hack: value of leaf == height of represented peak
(comment (node (leaf 1) (leaf 0) 0))
(S-n 3)

;; fresh attempt (16.11.2021)
;; reproduce the bagging structure by always recalculating from scratch. this is the first step in iteration towards moving towards a cached model

(defn create-new-range? [[n-2 n-1] n]
  (or (and (= 1 n-1) (= 0 n))
      (and (= 0 n-2) (= 1 n-1))))

(let [running-range [[]]
      ;; running-range [[0 0 1] [1] [0 0 1]]
      ;; running-range [[0 0 1] [1] [0 0 1] [0 0]]
      flattened-running-range (flatten running-range)
      new-bit 0]
  (if (create-new-range? (drop (- (count flattened-running-range) 2) flattened-running-range) new-bit)
    ;; if true, create new range
    (conj (into [] running-range) [new-bit])
    ;; else, append to last current range
    (concat (drop-last running-range) [(conj (into [] (last running-range)) new-bit)])))

;; DONE: add handling of bitlength = 2
(defn append-to-belt-range [running-range new-bit]
  (let [flattened-running-range (flatten running-range)
        preceding-bits (drop (- (count flattened-running-range) 2) flattened-running-range)
        preceding-bits-length (count preceding-bits)
        preceding-bits-padded (if (< preceding-bits-length 2)
                                (concat (repeat (- 2 preceding-bits-length) nil) preceding-bits)
                                preceding-bits)]
    (if (create-new-range? preceding-bits-padded new-bit)
      ;; if true, create new range
      (conj (into [] running-range) [new-bit])
      ;; else, append to last current range
      (concat (drop-last running-range) [(conj (into [] (last running-range)) new-bit)]))))

(append-to-belt-range [[0 0 1] [1] [0 0 1]] 0)

(bits-of-inc-n 10)

(defn belt-ranges [n]
  ;; NOTE: the values are in fact irrelevant - it's really just the lengths of these lists that matter.
  (reduce append-to-belt-range [[]] (rest (bits-of-n (inc n)))))

;; compare S-n-1 & S-n: see at what position difference results
;; compare belt-ranges n-1 & belt-ranges n: see at what position difference results

;; NOTE result: primitives/S-n & belt-ranges always have same flattened length
(every? #(apply = (map count %)) (map (juxt S-n (comp flatten belt-ranges)) (range 0 300)))

(defn children [node]
  (into [] (filter some?
                   ((juxt :core/left :core/right) node))))

(defn has-children? [node]
  (not-empty (children node)))

(s/fdef children
  :args (s/cat :node :core/node)
  ;; :ret (s/tuple ::parent (s/coll-of ::child))
  :ret (s/? (s/cat :left :core/left :right :core/right))
  :fn #(if (has-children? (second (:node (:args %))))
         (= 2 (count (:ret %)))
         ;; (do
         ;;   (if (not= 0 (count (:ret %))) (println %))
         ;;  true)
         (= 0 (count (:ret %)))))

(stest/instrument `children)
(stest/check `children)

