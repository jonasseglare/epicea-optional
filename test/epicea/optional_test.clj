(ns epicea.optional-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as spec]
            [epicea.optional :refer 
             [either optionally test-special
              compile-top expect defined? undefined? but]]
            [epicea.spec :refer [conform explain]] :reload-all))

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
                              :not-good)))))
  (is (= 9 (either (let [a (expect number? :not-number)]
                     (either a
                             ((fn [b] (either (+ 4 a) b)) 9)
                             :not-good))))))


(deftest quote-test
  (is (= '(a) (either (let [a (expect number? :a)] '(a))))))

(deftest try-catch-finally
  (is (= 9 (either (try 9))))
  (is (= :bad (either (try (assert false) (catch Throwable x :bad)))))
  (is (= :assertion
         (either (try (assert false) 
                      (catch java.lang.ArrayIndexOutOfBoundsException x :bad) 
                      (catch Throwable k :assertion)
                      (finally :really-bad))))))

(deftest vector-test
  (is (= [1 2 3] (either [1 (expect number? 2) 3] 4)))
  (is (= 4 (either [1 (expect number? :a) 3] 4))))

(deftest set-test2
  (let [k (either #{1 (expect number? 2) 3} 4)]
    (is (set? k))
    (is (= #{1 2 3} k)))
  (is (= 4 (either #{1 (expect number? :a) 3} 4))))

(deftest map-test
  (is (= {:a 3 :b 4} (either {:a 3 :b 4} 9)))
  (is (= 9 (either {:a (expect number? :asfd) :b 4} 9)))
  (is (= {:a 3 :b 4} (either {:a (expect number? 3) :b 4} 9))))

(deftest utilites
  (is (not (defined? (expect number? :a))))
  (is (true? (undefined? (expect number? :a))))
  (is (true? (defined? (expect number? 9))))
  (is (either (true? (but (expect number? :a)))
              false)))

(defn compute-bmi-sub [weight height]
  (/ weight (* height height)))

(defn compute-bmi [person]
  (either (compute-bmi-sub (expect number? (:weight person))
                           (expect number? (:height person)))
          nil))


(deftest bmi-test-basic
  (is (number? (compute-bmi {:weight 70 :height 1.73})))
  (is (not (number? (compute-bmi {:height 1.73})))))

(spec/def ::weight number?)
(spec/def ::height number?)
(spec/def ::person (spec/cat
                    :weight ::weight
                    :height ::height))

(defn compute-bmi-from-spec [pair]
  (either (compute-bmi (epicea.spec/conform ::person pair))
          nil))

(defn explain-person [x]
  (either (explain ::person x)
          nil))

(deftest bmi-test-basic
  (is (= (compute-bmi-from-spec [70.0 1.73])
         (compute-bmi {:weight 70.0 :height 1.73})))
  (is (nil? (explain-person [70.0 1.73])))
  (is (not (nil? (explain-person 3)))))

(deftest nesting
  (is (= 3 (either (expect number? (expect number? (expect number? 3)))
                   4)))
  (is (= 4 (either (expect number? (either (expect number? :a) 4))
                   5)))
  (is (= 5 (either (expect number? (either :a 4))
                   5))))

(defn KKKK [& args]
  (apply + args))

(deftest complex-macro
  (= "kattskit\n" (either (with-out-str (println "kattskit")))))


;;; Problem: This should evaluate to 7, but it evaluates to 10 (+ 3 (+ 3 4))
;;; (either (KKKK 3 (let* [x (expect number? 4)] x x)) nil)
