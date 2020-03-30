(ns chisel.curves-test
  (:require [clojure.test :refer :all]
            [chisel.curves :as curves]))

(deftest resolve-points-test
  (testing "Testing closed interval mapping of the resolve-points function"
    (is (= (curves/resolve-points 5 identity)
           '(0 1/4 1/2 3/4 1))))
  (testing "Testing start open interval mapping of the resolve-points function"
    (is (= (curves/resolve-points 5 identity :drop-first? true)
           '(1/5 2/5 3/5 4/5 1))))
  (testing "Testing end open interval mapping of the resolve-points function"
    (is (= (curves/resolve-points 5 identity :drop-last? true)
           '(0 1/5 2/5 3/5 4/5)))))
