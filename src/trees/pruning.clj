(ns trees.pruning
  (:require [trees.common :refer :all]
            [trees.measures :refer :all]))


;; WARN: the denominator in this expression is troubling
(defn g-score
  [node total]
  (/ (- (node-resubstitution-error node total) (subtree-resubstitution-error node total))
     (dec (tree-complexity node))))


(defn prune-iteration
  "Returns weakest links for a given iteration"
  [total tree]
  (let [scored-nodes (sort-by first (for [t (get-non-terminal-nodes tree)] [(g-score t total) t]))
        ;; blegh this won't actually work
        minimum-score (ffirst scored-nodes)
        weakest-links (map second (take-while #(= (first %) minimum-score) scored-nodes))]
    [minimum-score weakest-links]))


;; NOTE: this would be much easier with mutable trees :(
(defn prune-weakest-links
  [tree weak-links]
  (if (nil? tree)
    nil
    (if (some #(identical? tree %) weak-links)
      (dissoc tree :left :right)
      (let [left-pruned   (prune-weakest-links (:left tree) weak-links)
            right-pruned  (prune-weakest-links (:right tree) weak-links)]
        (cond (and (seq left-pruned) (seq right-pruned)) (assoc tree :left left-pruned :right right-pruned)
              (seq left-pruned)   (assoc tree :left left-pruned)
              (seq right-pruned)  (assoc tree :right right-pruned))))))


(defn get-initial-tree
  [tree total]
  (when (seq tree)
    (let [left  (:left tree)
          right (:right tree)]
      (if (and (terminal? left) (terminal? right)
               (= (node-resubstitution-error tree total)
                  (+ (node-resubstitution-error left total)
                     (node-resubstitution-error right total))))
        (dissoc tree :left :right)
        (let [left-init   (get-initial-tree left total)
              right-init  (get-initial-tree right total)]
          (cond (and (seq left-init) (seq right-init)) (assoc tree :left left-init :right right-init)
                (seq left-init) (assoc tree :left left-init)
                (seq right-init) (assoc tree :right right-init)
                :default tree))))))


(defn prune
  [total Tk]
  (when-not (terminal? Tk)
    (let [[alpha-k+1 weakest-links] (prune-iteration total Tk)
          Tk+1 (prune-weakest-links Tk weakest-links)]
      [alpha-k+1 Tk+1])))


(defn prune-tree
  [Tmax]
  (let [produce-pruned-tree (comp (partial prune (total-number Tmax)) second)]
    (take-while (complement nil?) (iterate produce-pruned-tree [0 Tmax]))))