(ns trees.tree-test
  (:require [clojure.test :refer :all]
            [trees.core :as t]
            [trees.dataframe :as df]))


(deftest get-majority-class
  (is (= (t/get-majority-class {:one 1 :two 2 :three 3 :four 4}) :four)))


(deftest node-misclassified-test
  (is (= (t/node-misclassified {:pos 10 :neg 100}) 10))
  (is (= (t/node-misclassified {:pos 10 :neg 100 :other 1000}) 110)))


(deftest satisfies-pred?
  (is (= (t/satisfies-pred? odd? (range 3 11)) [0 2 4 6]))
  (is (= (t/satisfies-pred? even? (range 3 11)) [1 3 5 7])))


(deftest create-leaf
  (is (= (t/create-leaf {:pos 10 :neg 100})
         {:leaf? true :prediction :neg :class-counts {:pos 10 :neg 100}})))


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
    (is (= (t/classify tree2 {"x1" 1 "x2" 1}) -1))))



(def sample-tree
  {:class-counts {:pos 100 :neg 100}
   :name :t1

   :left {:name :t2
          :class-counts {:pos 90 :neg 60}
          :left   {:name :t4
                   :class-counts {:pos 80 :neg 0}}
          :right  {:name :t5
                   :class-counts {:pos 10 :neg 60}
                   :left {:name :t8
                          :class-counts {:pos 0 :neg 60}}
                   :right {:name :t9
                           :class-counts {:pos 10 :neg 0}}}}

   :right {:name :t3
           :class-counts {:pos 10 :neg 40}
           :left {:name :t6
                  :class-counts {:pos 10 :neg 0}}
           :right {:name :t7
                   :class-counts {:pos 0 :neg 40}}}})


(deftest weakest-link-scoring
  (let [t1    sample-tree
        t2    (get-in sample-tree [:left])
        t3    (get-in sample-tree [:right])
        t5    (get-in sample-tree [:left :right])
        total 200]
    (is (= (t/g-score t1 total) 1/8))
    (is (= (t/g-score t2 total) 3/20))
    (is (= (t/g-score t3 total) 1/20))
    (is (= (t/g-score t5 total) 1/20))))