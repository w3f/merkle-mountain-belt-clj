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
      (range-position-flat m (butlast S-n) last-range-max-m (inc current-range)))))

(defn range-position-nested [m S-n]
  (let [range-position (range-position-flat m S-n 0 0)
        range-splits (range-splits S-n)
        ]
    (letfn [(range-position-nested-internal [range-position range-splits acc-ranges]
              (let [last-range-length (count (peek range-splits))]
                   (if (< range-position last-range-length)
                     [acc-ranges range-position]
                     (range-position-nested-internal (- range-position last-range-length) (pop range-splits) (inc acc-ranges))))
              )]
      (range-position-nested-internal range-position range-splits 0)
      )
    ))

(range-position-nested 150 (p/S-n 2000))

(let [m 3
      n 2000
      S-n (p/S-n n)
      range-splits (range-splits S-n)
      range-position-flat (range-position-flat m S-n 0 0)
      range-position-nested (range-position-nested m S-n)
      surrounding-range (nth range-splits (- (dec (count range-splits)) (first range-position-nested)))]
  [range-splits
   {:pos range-position-flat :height (nth S-n (- (dec (count S-n)) range-position-flat))}
   {:pos range-position-nested :height (nth surrounding-range (- (dec (count surrounding-range)) (second range-position-nested)))}
  (=
   (nth S-n (- (dec (count S-n)) range-position-flat))
   (nth (nth range-splits (- (dec (count range-splits)) (first range-position-nested)))
        (- (dec (count surrounding-range)) (second range-position-nested))))
   ]
  )

(let [n 2000
      S-n (p/S-n n)
      range-splits (range-splits S-n)]
  [(println (into [] S-n))
   (println (first range-splits))
   (= S-n (flatten (first range-splits)))])
