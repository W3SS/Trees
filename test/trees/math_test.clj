(ns trees.math-test
  (:require [clojure.test :refer :all]
            [trees.math :as m]))


(deftest find-min
         (let [xs [-2 -1 0 1 2]
               f #(* % %)]
           (is (= (m/min f xs) 0))
           (is (= (m/arg-min f xs) 0))))


(deftest find-max
  (let [xs [-2 -1 0 1 2]
        f identity]
    (is (= (m/max f xs) 2))
    (is (= (m/arg-max f xs) 2))))