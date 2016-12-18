(ns epicea.optional
  (:require [clojure.spec :as spec]))

(spec/def ::if-sym (constantly true)) ;#(= `if %))
(spec/def ::expr (constantly true))
(spec/def ::if-form (spec/cat :if-sym ::if-sym
                              :test ::expr
                              :on-true ::expr
                              :on-false (spec/? ::expr)))

(spec/def ::binding (spec/cat :symbol simple-symbol?
                              :expr ::expr))
(spec/def ::bindings (spec/* ::binding))
(spec/def ::form (constantly true))
(spec/def ::forms (spec/* ::form))
(spec/def ::let-symbol (constantly true)); #(= `let %))

(spec/def ::basic-let-form (spec/cat
                            :let-symbol ::let-symbol
                            :bindings (spec/spec ::bindings)
                            :forms ::forms))

(declare compile-sub)

(defn dissoc-many [m symbols]
  (reduce dissoc m symbols))

(defn wrap-dependent-sub-expr [symbols m cb-subexpr cb]
  (let [val-sym (gensym)
        test-sym (gensym)]
    `(let [~test-sym (and ~@symbols)
           ~val-sym (if ~test-sym ~(cb-subexpr (dissoc-many m symbols)))]
       ~(cb (assoc m val-sym test-sym) val-sym))))

(defn wrap-sub-expr [m dependencies cb-subexpr cb]
  (let [symbols (filter identity (map #(get m %) dependencies))]
    (if (empty? symbols)
      (cb m (cb-subexpr m))
      (wrap-dependent-sub-expr
       symbols m cb-subexpr cb))))

(defn either-sym? [f]
  (or (= 'either f) (= `either f)))

(defn optionally-sym? [f]
  (or (= 'optionally f) (= `optionally f)))

(def special-forms {'if :if ;; OK
                    'do :do
                    'let* :let
                    'loop :loop
                    'recur :recur
                    'fn :fn
                    'throw :throw
                    'try :try
                    'def :def
                    'var :var
                    'catch :catch
                    'monitor-enter :monitor-enter
                    'monitor-exit :monitor-exit})

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

 (defn compile-if-sub [m x cb]
   (compile-sub 
    m (:test x)
    (fn [m test-expr]
      (wrap-sub-expr 
       m [test-expr]
       (fn [m]
         `(if ~test-expr
            ~(compile-sub m (:on-true x) cb)
            ~(compile-sub m (:on-false x) cb))) 
       cb))))

(defn compile-if [m x0 cb]
  (let [x (spec/conform ::if-form x0)]
    (if (= ::spec/invalid x)
      (error (spec/explain ::if-form x0))
      (compile-if-sub m x cb))))

(defn compile-arg-list [acc m args cb]
  (if (empty? args)
    (cb m acc)
    (compile-sub 
     m (first args) 
     (fn [m expr]
       (compile-arg-list 
        (conj acc expr)
        m (rest args) cb)))))

(defn compile-fun-call [m x cb]
  (compile-arg-list 
   [] m (rest x)
   (fn [m arg-list]
     (wrap-sub-expr 
      m arg-list
      (fn [m]
        (cb m `(~(first x) ~@arg-list)))
      cb))))

(defn compile-let-sub [m bindings forms cb]
  (if (empty? bindings)
    (compile-sub
     m `(do ~@forms) cb)
    (let [[{:keys [symbol expr]} & rb] bindings]
      (compile-sub 
       m expr
       (fn [m x]
         `(let [~symbol ~x]
            ~(compile-let-sub
                 (if (contains? m x)
                   (assoc m symbol (get m x))
                   (dissoc m symbol))
               rb forms cb)))))))
              
                

(defn compile-let [m x0 cb]
  (let [x (spec/conform ::basic-let-form x0)]
    (if (= x ::spec/invalid)
      (error (spec/explain ::basic-let-form x0))
      (compile-let-sub m 
        (:bindings x) (:forms x) cb))))

(defn compile-do-sub [m forms cb]
  (cond
    (empty? forms) (cb m nil)
    (= 1 (count forms)) (compile-sub m (first forms) cb)
    :default 
    (compile-sub 
     m (first forms)
     (fn [m x]
       (wrap-sub-expr 
        m [x] 
        (fn [m] `(do ~x ~(compile-do-sub m (rest forms) cb)))
        cb)))))

    
    

(defn compile-do [m x cb]
  (compile-do-sub m (rest x) cb))

(defn compile-function-or-macro-call [m x cb]
  (let [expanded (macroexpand x)]
    (if (= expanded x)
      (compile-fun-call m x cb)
      (compile-sub m expanded cb))))

(defn compile-other-form [m x cb]
  (let [f (first x)
        k (get special-forms f)]
    (cond
      (= k :if) (compile-if m x cb)
      (= k :do) (compile-do m x cb)
      (= k :let) (compile-let m x cb)
      (contains? special-forms f) (cb m x)
      :default (compile-function-or-macro-call m x cb))))

(defn compile-seq [m x cb]
  (let [f (first x)]
    (cond
      (either-sym? f) (compile-either m x cb)
      (optionally-sym? f) (compile-optionally m x cb)
      :default (compile-other-form m x cb))))

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

(defmacro expect [test-fun expr]
  `(let [esym# ~expr]
     (optionally (~test-fun esym#) esym#)))
       
  
