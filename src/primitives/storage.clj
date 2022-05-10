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
