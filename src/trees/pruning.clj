(ns trees.pruning
  (:require [trees.measures :as m]))


(defn terminal?
  [node]
  (and (seq node)
       (nil? (:left node))
       (nil? (:right node))))




(defn get-initial-tree
  "Returns the tree T1 from Tmax to begin the minimal
  cost-complexity pruning procedure"
  [tree
   classes
   priors
   misclassification-cost
   class-counts]
  (let [cost-fn (fn [t] (m/node-cost t classes priors misclassification-cost class-counts))]
    (if (and (terminal? (:left tree))
             (terminal? (:right tree)))
      (if (= (cost-fn tree)
             (+ (cost-fn (:left tree)) (cost-fn (:right tree))))
        (dissoc tree :left :right)
        tree)
      (let [left-subtree (get-initial-tree (:left tree) classes priors misclassification-cost class-counts)
            right-subtree (get-initial-tree (:right tree) classes priors misclassification-cost class-counts)]
        (cond (and (seq left-subtree) (seq right-subtree)) (assoc tree :left left-subtree :right right-subtree)
              (seq left-subtree) (assoc tree :left left-subtree)
              (seq right-subtree) (assoc tree :right right-subtree))))))


(defn get-weakest-links
  "Returns 1 or more \"weakest\" link trees as defined by

  g_k (t*) = [R(t*) - R(T_t)] / [|~Tk| - 1], t* not in ~Tk "
  [Tk-1]
  ()
  )