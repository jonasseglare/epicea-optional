(ns epicea-optional.core)

(declare compile-sub)

(defn error [& s]
  (throw (RuntimeException. (apply str s))))

(defn compile-either-sub [m x cb]
  (cond
    (empty? x) (error "Either cannot be empty")
    (= 1 (count x)) (compile-sub m (first x) cb)
    :default (compile-either-sub m (rest x) cb)))

(defn compile-either [m x cb]
  (compile-either-sub m (rest x) cb))

(defn compile-seq [m x cb]
  (cond
    (= `either (first x)) (compile-either m x cb)
    :default (cb m x)))

(defn compile-sub [m x cb]
  (cond
    (seq? x) (compile-seq m x cb)
    :default x))


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
