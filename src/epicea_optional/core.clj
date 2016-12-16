(ns epicea-optional.core)

(defn error [& s]
  (throw (RuntimeException. (apply str s))))

(defn compile-either [m x cb]
  (cb m x))

(defn compile-seq [m x cb]
  (cond
    (= `either (first x)) (compile-either m x cb)
    :default (cb m x)))

(defn compile-sub [m x cb]
  (cond
    (seq? x) (compile-seq m x cb)
    :default nil))


(defn comptile-top [x]
  (println "Call compile top on " x)
  (compile-sub 
   {} x 
   (fn [m x]
     (if (contains? m x)
       (error "The top level form must not be optional: " x)
       x))))



(defmacro either [& x]
  (comptile-top `(either ~@x)))
