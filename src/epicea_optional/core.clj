(ns epicea-optional.core)

(defn error [& s]
  (throw (RuntimeException. (apply str s))))

(defn compile-seq [m x cb]
  (if (= `either (first x))
    (cb m :either)
    (cb m nil)))

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
