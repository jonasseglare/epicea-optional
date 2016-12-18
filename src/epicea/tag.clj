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
(def tag-value (tag :value))
(def undefined (tag :undefined nil))
(def value? (tagged? :value))

(defmacro wrap [expr]
  `(epicea.optional/either (tag-value ~expr) undefined))

(defmacro unwrap [expr]
  `(value (epicea.optional/expect value? ~expr)))
