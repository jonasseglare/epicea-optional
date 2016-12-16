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

(defn compile-optionally-sub [m [test-expr value-expr] cb]
  (let [t (gensym)
        v (gensym)]
    `(let [~t ~test-expr
           ~v (if ~t ~value-expr)]
       ~(cb (assoc m v t) v))))

(defn compile-optionally [m x cb]
  (let [args (rest x)]
    (if (= 2 (count args))
      (compile-optionally-sub m args cb)
      (error "optionally expects two arguments but got " x))))

(defn compile-other-form [m x cb]
  (cb m x))

(defn compile-seq [m x cb]
  (let [f (first x)]
    (cond
      (= `either f) (compile-either m x cb)
      (= `optionally f) (compile-optionally m x cb)
      :default (compile-other-form m (macroexpand x) cb))))

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
