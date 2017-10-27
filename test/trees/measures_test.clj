(ns trees.measures-test
  (:require [clojure.test :refer :all]
            [trees.measures :as m]))


(defn almost-equal
  [a b epsilon]
  (let [a'    (float (Math/abs (float a)))
        b'    (float (Math/abs (float b)))
        diff  (float (Math/abs (float (- a b))))]
    (cond (== a b) true
          (or (== a 0) (== b 0) (< diff Float/MIN_NORMAL)) (< diff (* epsilon Float/MIN_NORMAL))
          :else (< (Math/min (float (+ a' b')) Float/MAX_VALUE) epsilon))))


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



(deftest resubstitution-error
  (is (= 0.5 (m/resubstitution-error {:pos 400 :neg 400}))))


(deftest gini-index

  (is (= 0.16528923624803138 (m/gini-index {:pos 10 :neg 100})))
  (is (= 0.5 (m/gini-index {:pos 100 :neg 100})))
  (is (= 0.0 (m/gini-index {:pos 0 :neg 100})))
  (is (= (m/gini-index {:pos 5 :neg 50}) (m/gini-index {:pos 50 :neg 5}))))


;
;(deftest entropy
;  (is (= 0.30463607662577846 (m/entropy {:pos 10 :neg 100})))
;  (is (= 0.6931471805599453 (m/entropy {:pos 100 :neg 100})))
;  (is (= 0.0 (m/entropy {:pos 0 :neg 100})))
;  (is (= (m/entropy {:pos 5 :neg 50}) (m/entropy {:pos 50 :neg 5}))))


(deftest split-quality
  (let [impurity-measure  m/resubstitution-error
        root-impurity     (impurity-measure {:pos 400 :neg 400})

        quality-1 (m/split-quality impurity-measure root-impurity 0.5 {:pos 300 :neg 100} 0.5 {:pos 100 :neg 300})
        quality-2 (m/split-quality impurity-measure root-impurity 0.75 {:pos 200 :neg 400} 0.25 {:pos 200 :neg 0})]
    ;; values are "close", in theory these are actually equal
    (is (> 0.001 (Math/abs (float (- quality-1 quality-2))))))


  (let [impurity-measure  m/gini-index
        root-impurity     (impurity-measure {:pos 400 :neg 400})

        quality-1 (m/split-quality impurity-measure root-impurity 0.5 {:pos 300 :neg 100} 0.5 {:pos 100 :neg 300})
        quality-2 (m/split-quality impurity-measure root-impurity 0.75 {:pos 200 :neg 400} 0.25 {:pos 200 :neg 0})]
    ;; values are "close", in theory these are actually equal

    ;; FIXME: come up with better assertions for these
    (is (= quality-1 0.125))
    (is (= quality-2 0.1666666691501939))

    ;; Quality 2 should be better
    (is (< quality-1 quality-2))
    ))