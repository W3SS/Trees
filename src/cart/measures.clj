(ns cart.measures
  (:require [cart.math :as m]))


;; FIXME: use Kahan or recursive summation
(defn sum
  ""
  [xs]
  (reduce + xs))


(defn count-pred
  [p xs]
  (reduce (fn [acc x]
            (if (p x)
              (inc acc)
              acc))
          0))


(defn fall-into-node-with-class-j-probability
  "p(j,t) - Estimate for the probability that
   a case will both be in class j and fall into node t"
  [node
   class-j
   priors]
  )


(defn fall-into-node-probability
  "p(t) - Estimate for the probability that
   a case falls into node t"
  [node classes priors]
  (sum (for [class-j classes]
         (fall-into-node-with-class-j-probability node class-j priors))))


(defn class-j-given-node-probability
  "p(j|t) - Estimate of the probability that
   a case is in class j given that it falls into a node"
  [node class-j classes priors]
  (/ (fall-into-node-with-class-j-probability node class-j priors)
     (fall-into-node-probability node classes priors)))


(defn node-cost
  "r(t)"
  [node
   misclassification-cost
   classes]
  (m/min (fn [class-i]
           (sum (for [class-j classes
                      :when (not= class-j class-i)]
                  (* (misclassification-cost class-i class-j)
                     (class-j-given-node-probability node class-j classes priors)))))
         classes))


(defn tree-complexity
  "A tree's complexity is defined as the
  number of its terminal nodes"
  [tree]
  (if (and (nil? (:left tree)) (nil? (:right tree)))
    1
    (+ (recur (:left tree))
       (recur (:right tree)))))


(defn get-terminal-nodes
  [tree acc]
  (if (and (nil? (:left tree))
           (nil? (:right tree)))
    (conj acc tree)
    (concat (get-terminal-nodes (:left tree) acc)
            (get-terminal-nodes (:right tree) acc))))


(defn tree-cost
  "R(T)"
  [tree
   classes
   priors
   misclassification-cost]
  (sum
    (for [node (get-terminal-nodes tree [])]
      (* (node-cost node misclassification-cost classes)  ;; r(t)
         (fall-into-node-probability node classes priors)))))  ;; p(t)


(defn pruning-objective
  "The \"Minimum Cost Complexity\" Tree Objective as defined in CART"
  [tree alpha]
  (+ ;; R(T)
    (tree-cost tree classes priors misclassification-cost)
    ;; a*|~T|
    (* alpha (tree-complexity tree))))

