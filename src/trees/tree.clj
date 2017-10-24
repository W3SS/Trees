(ns trees.tree
  "Simple proof-of-concept of decision tree learning in pure Clojure"
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            [trees.dataframe :as df]
            [trees.math :refer [sum]]
            [taoensso.timbre :as log])
  (:import (clojure.lang ILookup)))


;; NOTE: this namespace assumes binary-encoded features for decision-trees.
;; You have to pre-binarize attributes before learning the tree.
(defn get-majority-class
  "Finds the key with highest value in the map"
  [counts]
  (first (apply max-key second counts)))


(defn node-misclassified
  "Sums the counts of non-majority classes within a node"
  [class-counts]
  (sum (vals (dissoc class-counts (get-majority-class class-counts)))))


(defn total-number
  [root]
  (sum (vals (:class-counts root))))


(defn node-resubstitution-error
  "R(t)"
  [node total]
  (/ (node-misclassified (:class-counts node)) total))


(defn terminal?
  [node]
  (and (nil? (:left node))
       (nil? (:right node))))


(defn non-terminal?
  [node]
  (or (seq (:left node))
      (seq (:right node))))


(defn get-nodes-pred
  [tree pred]
  (if (nil? tree)
    ()
    (let [left-nodes  (get-nodes-pred (:left tree) pred)
          right-nodes (get-nodes-pred (:right tree) pred)
          all-nodes   (concat left-nodes right-nodes)]
      (if (pred tree)
        (cons tree all-nodes)
        all-nodes))))


;; FIXME: can do this iteratively in a much safer way
(defn get-terminal-nodes
  [tree]
  (get-nodes-pred tree terminal?))



(defn get-non-terminal-nodes
  [tree]
  (get-nodes-pred tree non-terminal?))


(defn tree-complexity
  "A tree's complexity is defined as the
  number of its terminal nodes"
  [tree]
  (cond (nil? tree) 0
        (terminal? tree) 1
        :else (+ (tree-complexity (:left tree))
                 (tree-complexity (:right tree)))))


(defn subtree-resubstitution-error
  [subtree total]
  (sum
    (for [t (get-terminal-nodes subtree)]
      (node-resubstitution-error t total))))


;; WARN: the denominator in this expression is troubling
(defn g-score
  [node total]
  (/ (- (node-resubstitution-error node total) (subtree-resubstitution-error node total))
     (dec (tree-complexity node))))


(defn prune-iteration
  "Returns weakest links for a given iteration"
  [tree total]
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


;; STUBBED FOR NOW
(defn get-initial-tree
  [x]
  x)


(defn prune-tree
  "Generates an upwards sequence of pruned trees starting from T1 <= Tmax"
  [Tmax]
  (let [total (total-number Tmax)]
    (loop [alpha-k 0
           Tk Tmax
           acc [[alpha-k Tk]]]
      (if (terminal? Tk)
        acc
        (let [[alpha-k+1 weakest-links] (prune-iteration Tk total)
              Tk+1 (prune-weakest-links Tk weakest-links)]
          (recur alpha-k+1 Tk+1 (conj acc [alpha-k+1 Tk+1])))))))




(defn satisfies-pred?
  "Returns the indices of values which matched the predicate"
  [p xs]
  (keep-indexed (fn [i v] (when (p v) i)) xs))


(defn train-test-split
  "Shuffles data, splits df into two based on test reserve size"
  [df test-reserve]
  (let [N             (df/df-count df)
        indices       (shuffle (range N))
        test-size     (long (* N test-reserve))]
    [(df/select-by-indices-df df (drop test-size indices))
     (df/select-by-indices-df df (take test-size indices))]))

;; TODO: cross-validation


;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;                    Decision Tree Stuff
;;
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defn sub-branch-mistakes
  [data indices target]
  (-> data
      (df/select-by-indices-df indices)
      (df/get-attribute-values target)
      (frequencies)
      (node-misclassified)))


(defn find-best-splitting-feature
  "Searches across all features to find the one with lowest error"
  [data features target]
  (let [target-values   (df/get-attribute-values data target)
        num-data-points (float (df/df-count data))]
    ;; Loop through each feature to consider splitting on that feature
    (reduce (fn [acc feature]
              (let [best-error      (:error acc)
                    best-feature    (:feature acc)
                    feature-values  (df/get-attribute-values data feature)
                    left-mistakes   (sub-branch-mistakes data (satisfies-pred? zero? feature-values) target)
                    right-mistakes  (sub-branch-mistakes data (satisfies-pred? #(= % 1) feature-values) target)
                    error           (/ (+ left-mistakes right-mistakes) num-data-points)]
                (if (< error best-error)
                  {:error error :feature feature}
                  acc)))
            {:error 2.0 :feature nil}
            features)))


(defn create-leaf
  "Given a mapping from class -> count in node,
  creates a tree leaf node"
  [class-counts]
  {:prediction (get-majority-class class-counts)
   :leaf? true
   :class-counts class-counts})


(defn learn
  ([data features target] (learn data features target 0 10))
  ([data features target current-depth max_depth]
   (df/validate! data)
   (when (not (set? features))
     (throw (IllegalArgumentException. "features must be a set")))
    ;; Stopping condition 1:
    ;;   Check if there are mistakes at current node.
    ;; Stopping condition 2 (check if there are remaining features to consider splitting on)
    ;;   If there are no remaining features to consider, make current node a leaf node
    ;; Stopping condition 3:
    ;;   Reached maximum depth

   (let [target-values (df/get-attribute-values data target)
         _ (log/debug "--------------------------------------------------------------------")
         _ (log/debug (format "Subtree, depth = %s (%s data points)." current-depth (count target-values)))

         class-counts (frequencies target-values)]
     (cond (zero? (node-misclassified class-counts)) (do (log/debug "Stopping condition 1 reached.")
                                                          ;; If not mistakes at current node, make current node a leaf node
                                                          (create-leaf class-counts))

           (empty? features) (do (log/debug "Stopping condition 2 reached.")
                                 (create-leaf class-counts))

           (>= current-depth max_depth) (do (log/debug "Reached maximum depth. Stopping for now.")
                                            (create-leaf class-counts))

           :default (let [_ (log/debug "FELL THROUGH ALL")
                          result              (find-best-splitting-feature data features target)
                          splitting-feature   (:feature result)
                          splitting-error     (:error result)
                          fvs                 (df/get-attribute-values data splitting-feature)
                          left-split          (df/select-by-indices-df data (satisfies-pred? zero? fvs))
                          right-split         (df/select-by-indices-df data (satisfies-pred? #(= % 1) fvs))
                          remaining-features  (disj features splitting-feature)]

                      (log/debug (format "Split on feature %s. (%s, %s)" splitting-feature (df/df-count left-split) (df/df-count right-split)))

                      ;; Create a leaf node if the split is "perfect"
                      (cond (= (df/df-count left-split) (df/df-count data))   (do
                                                                                (log/debug "Creating leaf node.")
                                                                                (create-leaf (frequencies (df/get-attribute-values left-split target))))
                            (= (df/df-count right-split) (df/df-count data))  (do
                                                                                (log/debug "Creating leaf node.")
                                                                                (create-leaf (frequencies (df/get-attribute-values right-split target))))
                            :default (let [left-tree  (learn left-split remaining-features target (inc current-depth) max_depth)
                                           right-tree (learn right-split remaining-features target (inc current-depth) max_depth)]
                                       {:leaf? false
                                        :left left-tree
                                        :right right-tree
                                        :class-counts class-counts
                                        :splitting-feature splitting-feature})))))))


(defn classify
  "Given a fully constructed decision tree T and
  a record X, return the predicted label"
  [tree x]
  (if (:leaf? tree)
    (:prediction tree)
    (if (zero? (get x (:splitting-feature tree)))
        (recur (:left tree) x)
        (recur (:right tree) x))))



