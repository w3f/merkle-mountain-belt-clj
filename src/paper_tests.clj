(ns paper-tests
  (:require
   [clojure.test :refer [deftest testing is]]
   [primitives.core :refer [S-n]]
   [primitives.storage :refer [leaf-location storage-maps]]
   [primitives.proof :refer [type-rank parent-type-contenders child-type get-parent get-child get-sibling co-path-ephemeral co-path-internal co-path-internal-v0 sibling-index]]
   [linked-peaks :refer [play-algo]]))

(defn peaks-from-state [state currentP acc]
  (let [currentPstate (get (:node-map state) currentP)
        priorP (:left currentPstate)
        acc-current (conj acc currentPstate)]
    (if priorP
      (peaks-from-state state priorP acc-current)
      acc-current)))

(defn S-n-from-state [n]
  (let [state (play-algo n false)
        lastP (get state :lastP)]
    (->> (peaks-from-state state lastP [])
         (map :height)
         (reverse)
         (rest))))

(def S-n-from-state-1337 (S-n-from-state 1337))

;; Figure 5
(list
 (testing "Figure 5: Reproduce UMMB structure: S9 = (2, 2, 0)"
   (is (= (S-n 9) (S-n-from-state 9) '(2 2 0))))
 (testing "Figure 6"
   (is (= (S-n 9) (S-n-from-state 9) '(2 2 0)))
   (is (= (S-n 10) (S-n-from-state 10) '(2 2 1)))
   (is (= (S-n 11) (S-n-from-state 11) '(3 1 0))))
 (testing "Table 4"
   (is (=
        (map S-n (range 1 (inc 10)))
        (map S-n-from-state (range 1 (inc 10)))
        '((0) (1) (1 0) (1 1) (2 0) (2 1) (2 1 0) (2 1 1) (2 2 0) (2 2 1)))))
 (testing "Figure 8"
   (is (= (S-n 1337) S-n-from-state-1337 '(9 9 7 6 6 5 4 2 2 0))))
 (testing "Figure 9"
   (is (=
        (map S-n (range 12 (inc 19)))
        (map S-n-from-state (range 12 (inc 19)))
        '((3 1 1) (3 2 0) (3 2 1) (3 2 1 0) (3 2 1 1) (3 2 2 0) (3 2 2 1) (3 3 1 0)))))
 (testing "Figure 10"
   (is (and (= (S-n 1337) '(9 9 7 6 6 5 4 2 2 0))
            S-n-from-state-1337
            (=
             (primitives.core/binary-repr-of-n (inc 1337))
             "10100111010"))))
 (testing "S-n: 11, 1337, 91107"
   (is (and (= (S-n 11) (S-n-from-state 11) '(3 1 0))
            (= (S-n 1337) S-n-from-state-1337 '(9 9 7 6 6 5 4 2 2 0))
            (= (S-n 91107) '(15 15 14 12 11 10 10 9 8 7 6 4 3 3 1 0)))))
 (testing "reproduce multiple aspects of Figure 12 (S-n, bagging, range splits)"
   (is (and (= (S-n 91107) '(15 15 14 12 11 10 10 9 8 7 6 4 3 3 1 0))
            (= (S-n 91145) '(15 15 14 12 11 11 9 8 7 6 5 4 4 2 2 0))))))

(map S-n (range 1 (inc 10)))
