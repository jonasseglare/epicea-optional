(ns epicea-optional.core-test
  (:require [clojure.test :refer :all]
            [epicea-optional.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 9 (either 9)))
    (is (= 4 (either (optionally false 3) 4)))
    (is (= 3 (either (optionally true 3) 4)))))
