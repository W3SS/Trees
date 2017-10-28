(ns trees.measures
  (:require [trees.common :refer [get-terminal-nodes terminal? get-majority-class]]
            [trees.math :as m :refer [sum]]
            [taoensso.timbre :as log]))


(defn tree-complexity
  "A tree's complexity is defined as the
  number of its terminal nodes"
  [tree]
  (cond (nil? tree) 0
        (terminal? tree) 1
        :else (+ (tree-complexity (:left tree))
                 (tree-complexity (:right tree)))))

(defn node-misclassified
  "Sums the counts of non-majority classes within a node"
  [class-counts]
  (sum (vals (dissoc class-counts (get-majority-class class-counts)))))


(defn node-resubstitution-error
  "R(t)"
  [node total]
  (/ (node-misclassified (:class-counts node)) total))


(defn subtree-resubstitution-error
  [subtree total]
  (sum
    (for [t (get-terminal-nodes subtree)]
      (node-resubstitution-error t total))))




(defn accuracy
  "Compute accuracy"
  [truth predictions]
  (float (/ (sum (map #(if (= %1 %2) 1 0) truth predictions))
            (count truth))))


(defn resubstitution-error
  [class-counts]
  (let [vs (vals class-counts)]
    (- 1 (/ (apply max vs) (sum vs)))))


;; FIXME: apply smoothing to prevent 0 probabilities
(defn gini-index
  "AKA gini impurity"
  [class-counts]
  ;; FIXME: hack until I figure out what's up
  (if (= (count class-counts) 2)
    (let[A (second (first class-counts))
         B (second (second class-counts))
         N (+ A B)]
      ;;(println (/ A N) "*" (/ B N) "=" (* (/ A N) (/ B N)))
      (float (* (/ A N) (/ B N))))
  (let [total (sum (vals class-counts))
        probs (for [[k c] class-counts
                    :let [relative-frequency (/ c total)]]
                (do ;;(println relative-frequency "* (1 - " relative-frequency ")" " = " (* relative-frequency (- 1 relative-frequency)))
                  (* relative-frequency (- 1 relative-frequency))))]
    ;;(println "CLASS COUNTS: " class-counts)
    ;;(println "sum = "(sum (vec probs)))
    (sum probs))))


;; FIXME: log(0) is possible here, make a correction for zero probabilities
(defn entropy
  "AKA information gain"
  [class-counts]
  (let [total (sum (vals class-counts))]
    (* -1.0 (sum (for [[k c] class-counts
                       :let [relative-frequency (float (/ c total))]]
                   (* relative-frequency (Math/log relative-frequency)))))))


(defn split-quality
  [impurity-measure root-impurity left-proportion left-class-counts right-proportion right-class-counts]
  ;; delta_i(s,t) = i(t) - pi(l)i(l) - pi(r)i(r)
  ;(log/debug "ROOT:" root-impurity)
  ;;(log/debug "LEFT" left-class-counts " PROPORTION: " left-proportion " IMPURITY: " (impurity-measure left-class-counts))
  ;(log/debug "RIGHT:" right-class-counts "PROPORTION: " right-proportion " IMPURITY: " (impurity-measure right-class-counts))
  (- root-impurity
     (* left-proportion (impurity-measure left-class-counts))
     (* right-proportion (impurity-measure right-class-counts))))