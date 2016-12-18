(ns epicea.tag)

(defn tagged?
  ([tag] 
   (fn [x]
     (tagged? tag x)))
  ([tag x]
   (if (vector? x)
     (let [[a b] x]
       (= a tag)))))
