(ns epicea.optional)

(declare compile-sub)

(defn either-sym? [f]
  (or (= 'either f) (= `either f)))

(defn optionally-sym? [f]
  (or (= 'optionally f) (= `optionally f)))

(def special-forms {'if :if
                    'do :do
                    'loop :loop
                    'var :var
                    'let :let
                    'fn :fn
                    'throw :throw
                    'try :try
                    'catch :catch
                    'monitor-enter :monitor-enter
                    'monitor-exit :monitor-exit
                    'recur :recur
                    'def :def})

(defn error [& s]
  (throw (RuntimeException. (apply str s))))

(defmacro optionally [& args]
  (error "optionally called outside of select on args: " args))

(declare compile-either-sub)

(defn compile-either-branch [m x cb]
  (let [[f & r] x]
    (compile-sub 
     m f
     (fn [m v]
       (if (contains? m v)
         `(if ~(get m v) ~v ~(compile-either-sub m r cb))
         (cb m v))))))

(defn compile-either-sub [m x cb]
  (cond
    (empty? x) (error "Either cannot be empty")
    (= 1 (count x)) (compile-sub m (first x) cb)
    :default (compile-either-branch m x cb)))

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

(defn compile-if [m x cb]
  (error "TODO")
  (cb m x))

(defn compile-arg-list [acc m args cb]
  (println "Compile arg list on " acc m args cb)
  (if (empty? args)
    (cb m acc)
    (compile-sub 
     m (first args) 
     (fn [m expr]
       (println "Was called")
       (compile-arg-list 
        (conj acc expr)
        m
        (rest args) cb)))))

(defn wrap-arg-check [m arg-list wrapped]
  (let [symbols (filter identity (map #(get m %) arg-list))]
    (if (empty? symbols)
      wrapped
      `(if (and ~@symbols)
         ~wrapped))))

(defn compile-fun-call [m x cb]
  (compile-arg-list 
   [] m (rest x)
   (fn [m arg-list]
     (wrap-arg-check 
      m arg-list
      (cb m `(~(first x) ~@arg-list))))))
   
     

(defn compile-other-form [m x cb]
  (let [f (first x)
        k (get special-forms f)]
    (println "Got " x " which is has special form" k)
    (cond
      (= k :if) (compile-if m x cb)
      (contains? special-forms f) (cb m x)
      :default (compile-fun-call m x cb))))

(defn compile-seq [m x cb]
  (let [f (first x)]
    (cond
      (either-sym? f) (compile-either m x cb)
      (optionally-sym? f) (compile-optionally m x cb)
      :default (compile-other-form m (macroexpand x) cb))))

(defn compile-sub [m x cb]
  (cond
    (seq? x) (compile-seq m x cb)
    :default (cb m x)))


(defn compile-top [x]
  (compile-sub 
   {} x 
   (fn [m x]
     (if (contains? m x)
       (error "The top level form must not be optional: " x)
       x))))



(defmacro either [& x]
  (compile-top `(either ~@x)))

(defmacro test-special [x]
  (if (contains? special-forms (first x))
    x nil))

