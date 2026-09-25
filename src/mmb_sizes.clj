(ns mmb-sizes
  "MMB / U-MMB membership-proof sizes for (n, k) pairs, from the paper's reference
   functions (proof-size/proof-size, proof-size/ummb-proof-size).

   Conventions: n = number of leaves; k = 1 for the newest leaf, k = n for the oldest
   (the paper's convention; Lemma lem:a-mmb has sigma(1) = 17/8).  proof-size's k=1..n
   matches the co-path reference table studies/snowbridge/data/k-vs-n-no-phantom-512.csv row-for-row
   (paper-test/proof-size-calibration-test).

   Usage (from the repository root, e.g. inside `nix develop`):
     clojure -M:mmb-sizes pairs.csv out.csv         ; pairs.csv: header \"n,k\" then rows
     clojure -M:mmb-sizes --calibrate NMAX out.csv  ; rows n=1..NMAX: proof sizes for k=1..n
   Used by studies/snowbridge/analyse.py."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [proof-size :refer [proof-size ummb-proof-size]]))

(defn parse-pairs [path]
  (->> (str/split-lines (slurp path))
       rest
       (remove str/blank?)
       (map #(mapv (fn [s] (Long/parseLong (str/trim s))) (str/split % #",")))))

(defn write-calibration [nmax out]
  (with-open [w (io/writer out)]
    (doseq [n (range 1 (inc nmax))]
      (.write w (str (str/join "," (map #(proof-size n %) (range 1 (inc n)))) "\n")))))

(defn write-sizes [pairs-path out]
  (let [pairs (parse-pairs pairs-path)]
    (with-open [w (io/writer out)]
      (.write w "n,k,mmb_items,ummb_items\n")
      (doseq [[n k] pairs]
        (.write w (str n "," k "," (proof-size n k) "," (ummb-proof-size n k) "\n"))))
    (count pairs)))

(defn -main [& args]
  (let [[a b c] args]
    (cond
      (= a "--calibrate") (do (write-calibration (Long/parseLong b) c) (println "wrote" c))
      (and a b) (println "wrote" b (write-sizes a b) "pairs")
      :else (binding [*out* *err*]
              (println "usage: clojure -M:mmb-sizes pairs.csv out.csv | --calibrate NMAX out.csv")
              (System/exit 2)))
    (shutdown-agents)))
