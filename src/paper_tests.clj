(ns paper-tests
  (:require
   [clojure.test :refer [deftest testing is]]
   [primitives.core :refer [S-n]]
   [primitives.storage :refer [leaf-location storage-maps]]
   [primitives.proof :refer [type-rank parent-type-contenders child-type get-parent get-child get-sibling co-path-ephemeral co-path-internal co-path-internal-v0 sibling-index]]))

;; Figure 5
(list
 (testing "Figure 5: Reproduce UMMB structure: S9 = (2, 2, 0)"
   (is (= (S-n 9) '(2 2 0))))
 (testing "Figure 6"
   (is (= (S-n 9) '(2 2 0)))
   (is (= (S-n 10) '(2 2 1)))
   (is (= (S-n 11) '(3 1 0))))
 (testing "Table 4"
   (is (=
        (map S-n (range 1 (inc 10)))
        '((0) (1) (1 0) (1 1) (2 0) (2 1) (2 1 0) (2 1 1) (2 2 0) (2 2 1)))))
 (testing "Figure 8"
   (is (= (S-n 1337) '(9 9 7 6 6 5 4 2 2 0))))
 (testing "Figure 9"
   (is (=
        (map S-n (range 12 (inc 19)))
        '((3 1 1) (3 2 0) (3 2 1) (3 2 1 0) (3 2 1 1) (3 2 2 0) (3 2 2 1) (3 3 1 0)))))
 (testing "Figure 10"
   (is (= (S-n 1337) '(9 9 7 6 6 5 4 2 2 0))
       (=
        (primitives.core/binary-repr-of-n (inc 1337))
        "10100111010")))
 (testing "Figure 11"
   (is (= (map S-n)))))

(map S-n (range 1 (inc 10)))

