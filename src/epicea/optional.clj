(ns epicea.optional
  (:require [clojure.spec :as spec]))

(spec/def ::if-sym #(= 'if %))
(spec/def ::expr (constantly true))
(spec/def ::if-form (spec/cat :if-sym ::if-sym
                              :test ::expr
                              :on-true ::expr
                              :on-false (spec/? ::expr)))

(spec/def ::binding (spec/cat :symbol symbol?
                              :expr ::expr))
(spec/def ::bindings (spec/spec (spec/* ::binding)))
(spec/def ::form (constantly true))
(spec/def ::forms (spec/* ::form))
(spec/def ::let-symbol (constantly true)); #(= `let %))

(spec/def ::basic-let-form (spec/cat
                            :let-symbol ::let-symbol
                            :bindings ::bindings
                            :forms ::forms))

(spec/def ::loop-symbol (constantly true))

(spec/def ::loop-form (spec/cat
                       :loop-symbol ::loop-symbol
                       :bindings ::bindings
                       :forms ::forms))

(spec/def ::fn-symbol (constantly true))
(spec/def ::fn-name symbol?)

(spec/def ::fn-args (spec/spec
                     (spec/coll-of symbol?)))


(spec/def ::fn-arity (spec/spec
                      (spec/cat
                       :args ::fn-args
                       :forms ::forms)))


(spec/def ::fn-form (spec/cat
                     :fn-symbol ::fn-symbol
                     :fn-name (spec/? ::fn-name)
                     :fn-arities (spec/* ::fn-arity)))

(spec/def ::type (constantly true))

(spec/def ::finally-symbol #(= % 'finally))
(spec/def ::catch-symbol #(= % 'catch))

(spec/def ::catch-form (spec/spec
                        (spec/cat
                         :catch-symbol ::catch-symbol
                         :type ::type
                         :var-name symbol?
                         :forms ::forms)))

(spec/def ::finally-form (spec/spec
                          (spec/cat
                           :finally-symbol ::finally-symbol
                           :forms ::forms)))

(spec/def ::non-catch #(and (not (spec/valid? ::catch-form %))
                            (not (spec/valid? ::finally-form %))))

(spec/def ::try-form (spec/cat
                      :try-symbol symbol?
                      :forms (spec/* ::non-catch)
                      :catch-forms (spec/* ::catch-form)
                      :finally-form (spec/? ::finally-form)))

(declare compile-sub)

(defmacro dout [x]
  `(let [x# ~x]
     (println ~(str x) "=" x#)
     x#))


(defn dissoc-many [m symbols]
  (reduce dissoc m symbols))


(defn get-optional-test-symbols [m dependencies]
  (filter identity (map #(get m %) dependencies)))

(defn wrap-dependent-sub-expr [symbols m cb-subexpr cb]
  (let [val-sym (gensym)
        test-sym (gensym)]
    `(let [~test-sym (and ~@symbols)]
       (if ~test-sym
         ~(cb-subexpr (dissoc-many m symbols))
         ~(cb (assoc m test-sym test-sym) test-sym)))))

;; If all dependencies are good, call cb-subexpr only if those
;; dependencies are OK. Call cb with the output
(defn wrap-sub-expr [m dependencies cb-subexpr cb]
  (let [symbols (get-optional-test-symbols m dependencies)]
    (if (empty? symbols)
      (cb-subexpr m)
      (wrap-dependent-sub-expr
       symbols m cb-subexpr cb))))

(defn compile-arg-list [acc m args cb]
  (if (empty? args)
    (cb m acc)
    (compile-sub 
     m (first args) 
     (fn [m expr]
       (compile-arg-list 
        (conj acc expr)
        m (rest args) cb)))))

(defn compile-args-and-wrap [m x cb-wrapped cb-none]
  (compile-arg-list
   [] m x
   (fn [m arg-list]
     (wrap-sub-expr ;(with-dependents
      m arg-list
      (fn [m] (cb-wrapped m arg-list))
      cb-none))))


(defn either-sym? [f]
  (or (= 'either f) (= `either f)))

(defn optionally-sym? [f]
  (or (= 'optionally f) (= `optionally f)))

(def special-forms {'if :if ;; OK
                    'do :do ;; OK
                    'let* :let ;; OK
                    'loop* :loop ;; OK
                    'recur :recur ;; OK
                    'throw :throw ;; OK
                    'def :def ;; OK
                    'var :var ;; OK
                    'monitor-enter :monitor-enter ;; OK
                    'monitor-exit :monitor-exit ;; OK
                    'fn* :fn ;; OK
                    'try :try
                    'catch :catch
                    'quote :quote
                    })

(defn error [& s]
  (throw (RuntimeException. (apply str s))))

(defn return-defined [err]
  (fn [m x]
    (if (contains? m x)
      (error err)
      x)))

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

;; TODO: THE EXPRESSIONS MUST BE COMPILED, TO!!!
(defn compile-optionally-sub [m args cb]
  (let [t (gensym)
        v (gensym)]
    (compile-args-and-wrap
     m args
     (fn [m [test-expr value-expr]]
       `(let [~t ~test-expr
              ~v (if ~t ~value-expr)]
          ~(cb (assoc m v t) v)))
     cb)))

(defn compile-optionally [m x cb]
  (let [args (rest x)]
    (if (= 2 (count args))
      (compile-optionally-sub m args cb)
      (error "optionally expects two arguments but got " x))))


(defn with-dependent [m dep cb-expr cb]
  (if (contains? m dep)
    `(if ~(get m dep)
       ~(cb-expr (dissoc m dep))
       ~(cb m dep))
    (cb-expr m)))


 (defn compile-if-sub [m x cb]
   (compile-sub 
    m (:test x)
    (fn [m test-expr]
      (with-dependent m test-expr
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

(defn compile-fun-call [m x cb]
  (compile-args-and-wrap
   m x
   (fn [m arg-list]
     (cb m `(~@arg-list)))
   cb))

(defn compile-bindings [m bindings cb]
  (if (empty? bindings)
    (cb m)
    (let [[{:keys [symbol expr]} & rb] bindings]
      (compile-sub 
       m expr
       (fn [m x]
         `(let [~symbol ~x]
            ~(compile-bindings
                 (if (contains? m x)
                   (assoc m symbol (get m x))
                   (dissoc m symbol))
               rb cb)))))))

(defn compile-let-sub [m bindings forms cb]
  (compile-bindings
   m bindings
   (fn [m]
     (let [result (compile-sub m `(do ~@forms) cb)]
       result))))
                

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
       (if (contains? m x)
         `(if ~(get m x)
            (do ~x ~(compile-do-sub m (rest forms) cb))
            ~(cb m x))
         `(do ~x ~(compile-do-sub m (rest forms) cb)))))))

(defn compile-do [m x cb]
  (compile-do-sub m (rest x) cb))

(defn compile-function-or-macro-call [m x cb]
  (let [expanded (macroexpand x)]
    (if (= expanded x)
      (compile-fun-call m x cb)
      (do 
        (compile-sub m expanded cb)))))

(defn make-loop-bindings [bindings]
  (vec
   (mapcat 
    (fn [binding]
      (let [s (:symbol binding)]
        [s s]))
    bindings)))


(comment ;; OLD VERSION
  (defn return-from-loop [m expr] 
    (if (contains? m expr)
      `(if ~(get m expr) (vector ~expr))
      `(vector ~expr)))

  (defn compile-loop-sub [m x cb]
    (let [raw-sym (gensym)
          test-sym (gensym)
          loop-sym (gensym)
          bindings (:bindings x)
          test-symbols (get-optional-test-symbols m (map :symbol bindings))]
      (compile-bindings
       m (:bindings x)
       (fn [m]
         `(let [~raw-sym
                (if (and ~@test-symbols)
                  (loop ~(make-loop-bindings bindings)
                    ~(compile-sub 
                      (dissoc-many m test-symbols) `(do ~@(:forms x))
                      return-from-loop)))
                ~test-sym (vector? ~raw-sym)
                ~loop-sym (first ~raw-sym)]
            ~(cb (assoc m loop-sym test-sym) loop-sym)))))))

(defn return-from-loop [m expr]
  (if (contains? m expr)
    (error "Loop result values may not be optional. Consider expording it using 'export'.")
    expr))

(defn compile-loop-sub [m x cb]
  (let [raw-sym (gensym)
        test-sym (gensym)
        loop-sym (gensym)
        bindings (:bindings x)
        symbols (map :symbol bindings)
        test-symbols (get-optional-test-symbols m symbols)]
    (compile-bindings
     m (:bindings x)
     (fn [m]
       `(let [~test-sym (and ~@test-symbols)
              ~loop-sym
              (if ~test-sym
                (loop ~(make-loop-bindings bindings)
                  ~(compile-sub 
                    (dissoc-many m symbols) `(do ~@(:forms x))
                    return-from-loop)))]
          ~(cb (assoc m loop-sym test-sym) loop-sym))))))
          

(defn compile-loop [m x0 cb]
  (let [x (spec/conform ::loop-form x0)]
    (if (= x ::spec/invalid)
      (error (spec/explain ::loop-form x0))
      (compile-loop-sub 
       m x cb))))

(defn return-from-fn [m x]
  (if (contains? m x)
    (error "The return value of an fn must not be optional. Consider exporting it using 'export'.")
    x))

(defn compile-fn-arity [m x]
  (update-in
   x [:forms]
   (fn [forms]
     [(compile-sub (dissoc-many m (:args x))
                   `(do ~@forms) return-from-fn)])))

(defn compile-fn-sub [m x cb]
  (cb m (spec/unform
         ::fn-form
         (update-in
          x [:fn-arities]
          (fn [arities] (map #(compile-fn-arity m %) arities))))))

(defn compile-fn [m x0 cb]
  (let [x (spec/conform ::fn-form x0)]
    (if (= ::spec/invalid x)
      (error (spec/explain ::fn-form x))
      (compile-fn-sub m x cb))))

(defn compile-catch-form [m x]
  (update-in 
   x [:forms]
   (fn [forms]
     [(compile-sub
       (dissoc m (:var-name x))
       `(do ~@forms)
       (return-defined "Forms in catch must not be optional"))])))

(defn compile-catch-forms [m forms]
  (map #(compile-catch-form m %) forms))

(defn compile-finally-form [m x]
  (update-in 
   x [:forms]
   (fn [forms]
     (compile-sub 
      m `(do ~forms)
      (return-defined "The finally form cannot be optional")))))

(defn merge-many [& maps]
  (apply merge maps))

(defn compile-try-spec [m x]
  (merge-many
   x

   {:forms
    [(compile-sub
            m `(do ~@(:forms x)) 
            (return-defined "Forms inside try must not be optional"))]}

   {:catch-forms
    (compile-catch-forms m (:catch-forms x))}
   
   (if (contains? x :finally-form)
     {:finally-form (compile-finally-form m (:finally-form x))}
     {})))

(defn compile-try-sub [m x cb]
  (cb m (spec/unform
         ::try-form
         (compile-try-spec m x))))


(defn compile-try [m x0 cb]
  (let [x (spec/conform ::try-form x0)]
    (if (= ::spec/invalid x)
      (error (spec/explain ::try-form x0))
      (compile-try-sub m x cb))))

(defn compile-quote [m x cb]
  (cb m x))

(defn compile-other-form [m x cb]
  (let [f (first x)
        k (get special-forms f)]
    (cond
      (= k :if) (compile-if m x cb)
      (= k :do) (compile-do m x cb)
      (= k :let) (compile-let m x cb)
      (= k :loop) (compile-loop m x cb)
      (= k :fn) (compile-fn m x cb)
      (= k :try) (compile-try m x cb)
      (= k :quote) (compile-quote m x cb)

      ;; recur, throw, monitor-enter, monitor-exit:
      (contains? special-forms f) (compile-fun-call m x cb) 

      :default (compile-function-or-macro-call m x cb))))

(defn compile-seq [m x cb]
  (let [f (first x)]
    (cond
      (either-sym? f) (compile-either m x cb)
      (optionally-sym? f) (compile-optionally m x cb)
      :default (compile-other-form m x cb))))

(defn compile-coll [m x cb wrapper]
  (compile-args-and-wrap
   m x
   (fn [m arg-list]
     (cb m (wrapper arg-list)))
   cb))

(defn compile-vector [m x cb]
  (compile-coll m x cb vec))

(defn compile-set [m x cb]
  (compile-coll m x cb set))


(spec/def ::map-entry (spec/cat :key ::expr :value ::expr))
(spec/def ::map-entries (spec/* ::map-entry))

(defn get-flat-map-data [x]
  (apply
   concat
   (vec x)))

(defn make-map-from-flat-data [data]
  (let [values (spec/conform ::map-entries data)]
    (zipmap 
     (map :key values)
     (map :value values))))

(defn compile-map [m x cb]
  (compile-coll m (get-flat-map-data x) cb make-map-from-flat-data))

(defn compile-sub [m x cb]
  (cond
    (seq? x) (compile-seq m x cb)
    (vector? x) (compile-vector m x cb)
    (set? x) (compile-set m x cb)
    (map? x) (compile-map m x cb)
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

;; Just for debugging
(defmacro test-special [x]
  (if (contains? special-forms (first x))
    x nil))



;;;; Utilities 
(defmacro expect [test-fun expr]
  `(let [esym# ~expr]
     (optionally (~test-fun esym#) esym#)))

(defmacro defined? [expr]
  `(either (do ~expr true)
           false))

(defmacro undefined? [expr]
  `(either (do ~expr false)
           true))

(defmacro but [expr]
  `(expect true? (undefined? ~expr)))
