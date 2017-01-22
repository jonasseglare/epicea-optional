(ns epicea.tag)

(defn tagged?
  ([tag] 
   (fn [x]
     (tagged? tag x)))
  ([tag x]
   (if (vector? x)
     (let [[a b] x]
       (= a tag)))))

(defn value [x]
  (second x))

(defn tag 
  ([t x] [t x])
  ([t] (fn [x] (tag t x))))



;;; Convenience definitions  
(def tag-success (tag :success))
(def tag-failure (tag :failure))
(def undefined (tag :undefined nil))
(def success? (tagged? :success))
(def failure? (tagged? :failure))

;; Utilites for passing optional values between functions
(defmacro wrap [expr]
  `(epicea.optional/either (tag-success ~expr) undefined))

(defmacro unwrap 
  ([expr] `(value (epicea.optional/expect success? ~expr)))
  ([tag expr] `(value (epicea.optional/expect (tagged? ~tag) ~expr))))
