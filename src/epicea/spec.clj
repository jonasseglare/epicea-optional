(ns epicea.spec
  (:require [clojure.spec :as spec])
  (:require [epicea.optional :as opt]))

(defn not-invalid [x]
  (not= x ::spec/invalid))

;; Just like clojure.spec/conform, but only
;; defined if conform worked.
(defmacro conform [s x]
  `(opt/expect not-invalid (spec/conform ~s ~x)))

(defn successful-explanation [x]
  (= "Success!\n" x))

(defn no-successful-explanation [x]
  (not (successful-explanation x)))

(defn explain-str [s x]
  (with-out-str
    (spec/explain s x)))

;; (fn [x] (not (successful-explanation x)))

;; Like clojure.spec/explain, but
;;   * Returns the explanation as a string
;;   * The return value is optional and only defined if 
;;     it doesn't conform.
(defmacro explain [s x]
  `(opt/expect 
    no-successful-explanation
    (explain-str ~s ~x)))


  
