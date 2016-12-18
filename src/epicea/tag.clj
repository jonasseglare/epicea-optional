(ns epicea.tag)

(defn tagged?
  ([tag] 
   (fn [x]
     (tagged? tag x)))
  ([tag x]
   (if (vector? x)
     (let [[a b] x]
       (= a tag)))))

(defn tagged-value [x]
  (second x))

(defn tag 
  ([t x] [t x])
  ([t] (fn [x] (tag t x))))
  
