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