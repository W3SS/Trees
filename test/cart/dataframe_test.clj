(ns cart.core-test
  (:require [clojure.test :refer :all]
            [cart.dataframe :as df]))


(deftest dataframe-count
  (let [sample-dataframe {"A" {:values [1 2 3]
                               :storage-type :int
                               :domain-type :discrete}
                          "B" {:values [5.0 6.0 7.0]
                               :storage-type :float
                               :domain-type :continuous}}]
    (is (= (df/df-count sample-dataframe) 3))))