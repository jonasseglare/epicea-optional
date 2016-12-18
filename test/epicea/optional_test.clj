(ns epicea.optional-test
  (:require [clojure.test :refer :all]
            [epicea.optional :refer 
             [either optionally test-special
              compile-top expect]] :reload-all))

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
  (is (= 5 (test-special (let* [a 3] (+ a 2))))))

(defn add-3 [x]
  (either (+ 3 (optionally (number? x) x))
          nil))

(deftest mini-test
  (testing "Certain basic aspects"
    (is (= 7 (add-3 4)))
    (is (nil? (add-3 :a)))
    (is (nil? (add-3 nil)))))


(deftest if-test
  (testing "if form"
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
    (is (nil? (either (do (optionally false 1) 2) nil)))
    (let [x (atom false)]
      (is (= :bad (either (do (reset! x true) (optionally false 9) 19)
                          :bad)))
      (is (deref x)))))


(deftest let-test
  (testing "let form"
    (is (= 7 (either (let [a 3] (+ a 4)))))
    (is (= 4 (either (let [a (optionally false 3) b 4] (either a b)))))
    (is (= 19 (either (let [a (optionally false 3) b (* 2 a) c 19] (either b c)))))
    (is (= 6 (either (let [a (optionally true 3) b (* 2 a) c 19] (either b c)))))
    (is (= 4 (either (let [a (expect number? :a)] (let [a 4] a)))))))

(deftest expect-test
  (testing "expect"
    (is (= 3 (either (expect number? 3) 4)))
    (is (= 4 (either (expect number? :not-a-number) 4)))))


(deftest loop-test
  (is (= 6 (either (loop [a (expect number? 3)] (* 2 a)) :b)))
  (is (= 15 (either (loop [a (expect number? 5) sum 0] 
                      (if (= a 0) sum (recur (- a 1) (+ sum a)))) nil))))
  
(deftest def-var-test 
  (is (= 10 (either (do (def a 10)
                        (var a)
                        a)
                    nil))))

(deftest fn-test
  (is (= 7 ((either (fn rulle [a] (+ a 3))) 4)))
  (is (= 7 ((either (fn [a] (+ a 3))) 4)))
  (is (= 13 (either (let [a (expect number? :not-number)]
                      (either ((fn [a] (+ 4 a)) 9)
                              :not-good)))))
  (is (= 13 (either (let [a (expect number? :not-number)]
                      (either a
                              ((fn [a] (+ 4 a)) 9)
                              :not-good))))))

(comment  (is (= 9 (either (let [a (expect number? :not-number)]
                     (either a
                             ((fn [b] (either (+ 4 a) b)) 9)
                             :not-good))))))

