(ns cart.tree
  "Simple proof-of-concept of decision tree learning in pure Clojure"
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            [cart.dataframe :as df]
            [taoensso.timbre :as log])
  (:import (clojure.lang ILookup)))


;; NOTE: this namespace assumes binary-encoded features for decision-trees.
;; You have to pre-binarize attributes before learning the tree.


(defn count-pos-and-neg
  [xs]
  (reduce (fn [[pos neg] x]
            (if (= 1 x)
              [(inc pos) neg]
              [pos (inc neg)]))
          [0 0]
          xs))


(defn node-misclassified
  "Majority wins"
  [labels-in-node]
  (if (empty? labels-in-node)
    (long 0)
    (long (apply min (count-pos-and-neg labels-in-node)))))


(comment
  (def ex1 [-1, -1, 1, 1, 1])
  (def ex2 [-1, -1, 1, 1, 1, 1, 1])
  (def ex3 [-1, -1, -1, -1, -1, 1, 1])

  (assert (= (node-misclassified ex1) 2))
  (assert (= (node-misclassified ex2) 2))
  (assert (= node-misclassified ex3) 2))


(defn satisfies-pred?
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

(defn find-best-splitting-feature
  [data features target]
  (let [target-values   (df/get-attribute-values data target)
        num-data-points (float (df/df-count data))]
    ;; Loop through each feature to consider splitting on that feature
    (reduce (fn [acc feature]
              (let [;;_ (log/debug "CHOSEN FEATURE" feature)
                    best-error      (:error acc)
                    best-feature    (:feature acc)
                    fvs             (df/get-attribute-values data feature)

                    ;; left split contains points where feature value is zero
                    left-split      (df/select-by-indices-df data (satisfies-pred? zero? fvs))

                    ;; right split contains points where feature value is one
                    right-split     (df/select-by-indices-df data (satisfies-pred? #(= % 1) fvs))

                    A (df/get-attribute-values left-split target)
                    B (df/get-attribute-values right-split target)
                    left-mistakes   (node-misclassified A)
                    right-mistakes  (node-misclassified B)
                    error           (/ (+ left-mistakes right-mistakes) num-data-points)]
                (if (< error best-error)
                  {:error error :feature feature}
                  acc)))
            {:error 2.0 :feature nil}
            features)))


(defn create-leaf
  [target-values]
  ;; Create a leaf node
  (let [[num-pos num-neg] (count-pos-and-neg target-values)]
    (if (> num-pos num-neg)
      {:prediction :pos :leaf? true}
      {:prediction :neg :leaf? true})))


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
         _ (log/debug (format "Subtree, depth = %s (%s data points)." current-depth (count target-values)))]
     (cond (zero? (node-misclassified target-values)) (do (log/debug "Stopping condition 1 reached.")
                                                          ;; If not mistakes at current node, make current node a leaf node
                                                          (create-leaf target-values))

           (empty? features) (do (log/debug "Stopping condition 2 reached.")
                                 (create-leaf target-values))

           (>= current-depth max_depth) (do (log/debug "Reached maximum depth. Stopping for now.")
                                            (create-leaf target-values))

           :default (let [_ (log/debug "FELL THROUGH ALL")
                          result (find-best-splitting-feature data features target)
                          splitting-feature (:feature result)
                          splitting-error (:error result)
                          fvs                 (df/get-attribute-values data splitting-feature)
                          left-split          (df/select-by-indices-df data (satisfies-pred? zero? fvs))
                          right-split         (df/select-by-indices-df data (satisfies-pred? #(= % 1) fvs))
                          remaining-features  (disj features splitting-feature)]

                      (log/debug (format "Split on feature %s. (%s, %s)" splitting-feature (df/df-count left-split) (df/df-count right-split)))

                      ;; Create a leaf node if the split is "perfect"
                      (cond (= (df/df-count left-split) (df/df-count data))   (do
                                                                                (log/debug "Creating leaf node.")
                                                                                (create-leaf (df/get-attribute-values left-split target)))
                            (= (df/df-count right-split) (df/df-count data))  (do
                                                                                (log/debug "Creating leaf node.")
                                                                                (create-leaf (df/get-attribute-values right-split target)))
                            :default (let [left-tree  (learn left-split remaining-features target (inc current-depth) max_depth)
                                           right-tree (learn right-split remaining-features target (inc current-depth) max_depth)]
                                       {:leaf? false
                                        :left left-tree
                                        :right right-tree
                                        :splitting-feature splitting-feature})))))))


(defn classify
  "Given a fully constructed decision tree T and
  a record X, return the predicted label"
  [tree x]
  (if (:leaf? tree)
    (if (= (:prediction tree) :pos) 1 -1)
    (let [split_feature_value (get x (:splitting-feature tree))]
      (if (zero? split_feature_value)
        (recur (:left tree) x)
        (recur (:right tree) x)))))



