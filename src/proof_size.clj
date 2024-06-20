(ns proof-size
  (:require [primitives.core :as p]))

(defn range-splits [S-n]
  (first (reduce (fn [[acc last-elem but-last-elem] elem]
                   [(if (or (= last-elem but-last-elem) (= 2 (- last-elem elem)))
                      (conj acc [elem])
                      (conj (or (pop acc) []) (conj (last acc) elem)))
                    elem last-elem])
                 [[[]] 0 -1]
                 S-n)))

(defn range-position [m ranges acc-ranges acc-leaves]
  (let [range-splits ranges
        last-range-max-m (reduce (fn [acc elem] (+ acc (Math/pow 2 elem))) 0 (peek range-splits))
        acc-leaves (+ acc-leaves last-range-max-m)]
    (if (<= m acc-leaves)
      acc-ranges
      (range-position m (pop range-splits) (inc acc-ranges) acc-leaves)
      )
    ))

(range-position 80 (range-splits (p/S-n 2000)) 0 0)

(let [n 2000
      S-n (p/S-n n)
      range-splits (range-splits S-n)]
  [(println (into [] S-n))
   (println (first range-splits))
   (= S-n (flatten (first range-splits)))])
