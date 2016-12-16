(ns epicea-optional.core-test
  (:require [clojure.test :refer :all]
            [epicea-optional.core :refer [either optionally is-let? compile-top]]))

(deftest basic
  (testing "Basic testing"
    (is (= 9 (either 9)))
    (is (= 4 (either (optionally false 3) 4)))
    (is (= 3 (either (optionally true 3) 4)))
    (is (= 5 (either 5 (optionally true 3))))
    (is (= 5 (either (+ 2 3))))))

(deftest tests-with-exceptions
  (testing "Here there should be errors"
    (is (try 
          (compile-top `(optionally false 3))
          false
          (catch Throwable e
            true)))
    (is (try
          (compile-top `(either (optionally false 3)))
          false
          (catch Throwable e
            true)))))

(deftest special-syms
  (is (= 5 (is-let? (let [a 3] (+ a 2))))))

