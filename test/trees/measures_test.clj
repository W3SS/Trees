(ns trees.measures-test
  (:require [clojure.test :refer :all]
            [trees.measures :as m]))


(deftest fall-into-node-with-class-j-probability

  (let [node  {:class-counts {:pos 10 :neg 100}}
        prior {:pos 0.50 :neg 0.50}
        class-distribution {:pos 1000 :neg 1000}

        pos-prob (m/fall-into-node-with-class-j-probability node :pos prior class-distribution)
        neg-prob (m/fall-into-node-with-class-j-probability node :neg prior class-distribution)]

    (is (> neg-prob pos-prob))
    (is (= 0.05 neg-prob))
    (is (= 0.005 pos-prob))))


(deftest accuracy
  (let [y [0 0 0 0 1]
        yhat [0 0 0 0 0]]
    (is (= (float 0.8) (m/accuracy y yhat)))))



(deftest gini-index

  (is (= 0.16528923624803138 (m/gini-index {:pos 10 :neg 100})))
  (is (= 0.5 (m/gini-index {:pos 100 :neg 100})))
  (is (= 0.0 (m/gini-index {:pos 0 :neg 100})))
  (is (= (m/gini-index {:pos 5 :neg 50}) (m/gini-index {:pos 50 :neg 5}))))



(deftest entropy
  (is (= 0.30463607662577846 (m/entropy {:pos 10 :neg 100})))
  (is (= 0.6931471805599453 (m/entropy {:pos 100 :neg 100})))
  (is (= 0.0 (m/entropy {:pos 0 :neg 100})))
  (is (= (m/entropy {:pos 5 :neg 50}) (m/entropy {:pos 50 :neg 5}))))