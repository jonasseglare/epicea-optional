(ns epicea.tag-test
  (:require [clojure.test :refer :all]
            [epicea.tag :refer :all] :reload-all))

(deftest tags
  (is (tagged? :success [:success 3]))
  (is (not (tagged? :success [:failure 3]))))
