(ns epicea.optional-test
  (:require [clojure.test :refer :all]
            [epicea.optional :refer 
             [either optionally test-special
              compile-top]] :reload-all))

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
  (is (= 5 (test-special (let [a 3] (+ a 2))))))

(defn add-3 [x]
  (either (+ 3 (optionally (number? x) x))
          nil))

(deftest mini-test
  (testing "Certain basic aspects"
    (is (= 7 (add-3 4)))
    (is (nil? (add-3 :a)))
    (is (nil? (add-3 nil)))))


(deftest if-test
  (testing "if statement"
    (is (= 4 (either (if 3 4 5))))
    (is (= :bad (either (if (optionally false 3) 4 5) :bad)))
    (is (= 4 (either (if (optionally true 3) 4 5) :bad)))
    (is (= 5 (either (if (optionally true false) 4 5) :bad)))
    (is (= :bad (either (if (optionally true false) 4 (optionally false 8)) :bad)))
    (is (= 8 (either (if (optionally true false) 4 (optionally true 8)) :bad)))
    (is (= 20 (either (if (optionally true true) (optionally true 20) 19) :bad)))
    (is (= :bad (either (if (optionally true true) (optionally false 20) 19) :bad)))))

(deftest do-test
  (testing "do form"
    (is (= 2 (either (do (optionally true 1) 2) nil)))
    (is (nil? (either (do (optionally false 1) 2) nil)))))

;(defn add-3-expect [x]
;  (either (+ 3 (expect number? x))
;          nil))
