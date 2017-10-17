(ns cart.tree-test
  (:require [clojure.test :refer :all]
            [cart.tree :as t]))


(deftest classifies-xor
         (let [;; | o     x
               ;; |
               ;; | x     o
               ;;  ----------
               dataframe {:df/count 4
                          "x1" {:values [0 0 1 1]
                                :storage-type :int
                                :domain-type :discrete}
                          "x2" {:values [0 1 0 1]
                                :storage-type :int
                                :domain-type :discrete}
                          "y"  {:values [-1 1 1 -1]
                                :storage-type :int
                                :domain-type :discrete}}
               learned-tree (t/learn dataframe #{"x1" "x2"} "y")]
           (is (= (t/classify learned-tree {"x1" 0 "x2" 0}) -1))
           (is (= (t/classify learned-tree {"x1" 0 "x2" 1}) 1))
           (is (= (t/classify learned-tree {"x1" 1 "x2" 0}) 1))
           (is (= (t/classify learned-tree {"x1" 1 "x2" 1}) -1))))


(deftest classifies-axis-aligned-rectangles
  (let [;; | o     x
        ;; |
        ;; | o     x
        ;;  ----------
        rect1 {:df/count 4
               "x1" {:values [0 0 1 1]:storage-type :int :domain-type :discrete}
               "x2" {:values [0 1 0 1] :storage-type :int :domain-type :discrete}
               "y"  {:values [1 1 -1 -1] :storage-type :int :domain-type :discrete}}

        tree1 (t/learn rect1 #{"x1" "x2"} "y")

        ;; | x     x
        ;; |
        ;; | o     o
        ;;  ----------
        rect2 {:df/count 4
               "x1" {:values [0 0 1 1]:storage-type :int :domain-type :discrete}
               "x2" {:values [0 1 0 1] :storage-type :int :domain-type :discrete}
               "y"  {:values [1 -1 1 -1] :storage-type :int :domain-type :discrete}}

        tree2 (t/learn rect2 #{"x1" "x2"} "y")]

    (is (= (t/classify tree1 {"x1" 0 "x2" 0}) 1))
    (is (= (t/classify tree1 {"x1" 0 "x2" 1}) 1))
    (is (= (t/classify tree1 {"x1" 1 "x2" 0}) -1))
    (is (= (t/classify tree1 {"x1" 1 "x2" 1}) -1))


    (is (= (t/classify tree2 {"x1" 0 "x2" 0}) 1))
    (is (= (t/classify tree2 {"x1" 0 "x2" 1}) -1))
    (is (= (t/classify tree2 {"x1" 1 "x2" 0}) 1))
    (is (= (t/classify tree2 {"x1" 1 "x2" 1}) -1))

    ))