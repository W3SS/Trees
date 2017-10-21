(ns trees.math-test
  (:require [clojure.test :refer :all]
            [trees.math :as m]))


(deftest find-maximal-values
  (is (= [-2 4] (m/find-maximal-values #(* % %) [-2 -1 0 1 2])))
  (is (= [3 3] (m/find-maximal-values identity [0 1 2 3]))))


(deftest find-minimal-values
  (is (= [0 0] (m/find-minimal-values #(* % %) [-2 -1 0 1 2])))
  (is (= [0 0] (m/find-minimal-values identity [0 1 2 3]))))


(deftest find-min
         (let [xs [-2 -1 0 1 2]
               f #(* % %)]
           (is (= 0 (m/min f xs)))
           (is (= 0 (m/arg-min f xs)))))


(deftest find-max
  (let [xs [-2 -1 0 1 2]
        f identity]
    (is (= 2 (m/max f xs)))
    (is (= 2 (m/arg-max f xs)))))


(deftest sum
  (is (= 6 (m/sum [1 2 3])))
  (is (= 6.0 (m/sum [1.0 2.0 3.0]))))