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

(defn range-position-flat [m S-n acc-leaves current-range]
  (let [last-range-max-m (+ acc-leaves (Math/pow 2 (last S-n)))]
    (if (< m last-range-max-m)
      current-range
      (range-position-flat m (butlast S-n) (+ acc-leaves last-range-max-m) (inc current-range)))))

      )
    ))

(range-position 80 (range-splits (p/S-n 2000)) 0 0)

(let [n 2000
      S-n (p/S-n n)
      range-splits (range-splits S-n)]
  [(println (into [] S-n))
   (println (first range-splits))
   (= S-n (flatten (first range-splits)))])
