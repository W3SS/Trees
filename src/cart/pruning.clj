(ns cart.pruning
  (:require [cart.measures :refer [node-cost]]))


(defn terminal?
  [node]
  (and (seq node)
       (nil? (:left node))
       (nil? (:right node))))




(defn get-initial-tree
  [tree
   classes
   priors
   misclassification-cost]
  (let [cost-fn (fn [t] (node-cost t classes priors misclassification-cost))]
    (if (and (terminal? (:left tree))
             (terminal? (:right tree)))
      (if (= (cost-fn tree)
             (+ (cost-fn (:left tree)) (cost-fn (:right tree))))
        (dissoc tree :left :right)
        tree)
      (let [left-subtree (recur (:left tree) classes priors misclassification-cost)
            right-subtree (recur (:right tree) classes priors misclassification-cost)]
        (cond (and (seq left-subtree) (seq right-subtree)) (assoc tree :left left-subtree :right right-subtree)
              (seq left-subtree) (assoc tree :left left-subtree)
              (seq right-subtree) (assoc tree :right right-subtree))))))