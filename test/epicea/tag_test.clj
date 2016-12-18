(ns epicea.tag-test
  (:require [clojure.test :refer :all]
            [epicea.optional :refer :all]
            [epicea.tag :refer :all] :reload-all))

(deftest tags
  (is (tagged? :success [:success 3]))
  (is (not (tagged? :success [:failure 3])))
  (is (= 3 (value [:success 3])))
  (is (= [:success 3] (tag :success 3)))
  (is ((tagged? :success) [:success 3]))
  (is (not ((tagged? :success) [:failure 3])))
  (is (= [:success 3] ((tag :success) 3)))
  (is (= [:value 3] (wrap 3)))
  (is (= 4 (either (unwrap [:value 4])
                   9)))
  (is (= 9 (either (unwrap [:garbage 4])
                   9))))
