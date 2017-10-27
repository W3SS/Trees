(ns trees.measures
  (:require [trees.math :as m :refer [sum]]
            [taoensso.timbre :as log]))


;; Given a learning sample L for a J class problem
;; let N_j be the number of cases in class J
;;
;; We can estimate the priors to be {PI(j)} as {N_j/N} or assume known by the user
;;
;; The proportion of class j cases in L falling into t is N_j(t)/N_j
;;
;; In a node t, let N(t) be the total number of cases in L with x_n in t
;; let N_j(t) be the number of class j cases in t
;;
;; NOTE: rather than caching all this metadata about N_j(t) and such in a shared variable,
;; how about we just store this information locally at each node? This would make it easier to carry around as well.

(defn fall-into-node-with-class-j-probability
  "p(j,t) - Estimate for the probability that
   a case will both be in class j and fall into node t

   PI(j) * N_j(t) / N_j"
  [node                   ;; the tree node
   class-j                ;; label class of interest
   priors                 ;; {PI(j)} the set of prior probabilities on each class j
   class-counts           ;; the frequencies of classes in the whole dataset
   ]
  (* (get priors class-j) (/ (get-in node [:class-counts class-j])
                             (get class-counts class-j))))


(defn fall-into-node-probability
  "p(t) - Estimate for the probability that
   a case falls into node t"
  [node                   ;; the tree node
   classes                ;; all possible labels J = {j}
   priors                 ;; {PI(j)} the set of prior probabilities on each class j
   class-counts           ;; the frequencies of classes in the whole dataset
   ]
  (sum (for [class-j classes]
         (fall-into-node-with-class-j-probability node class-j priors class-counts))))


(defn class-j-given-node-probability
  "p(j|t) - Estimate of the probability that
   a case is in class j given that it falls into a node"
  [node           ;; the tree node
   class-j        ;; the class label j
   classes        ;; all possible labels J = {j}
   priors         ;; {PI(j)} the set of prior probabilities on each class j
   class-counts   ;; the frequencies of classes in the whole dataset
   ]
  (/ (fall-into-node-with-class-j-probability node class-j priors class-counts)
     (fall-into-node-probability node classes priors class-counts)))


(defn node-cost
  "r(t)"
  [node
   classes
   priors
   misclassification-cost
   class-counts             ;; the frequencies of classes in the whole dataset
   ]
  (m/min (fn [class-i]
           (sum (for [class-j classes
                      :when (not= class-j class-i)]
                  (* (misclassification-cost class-i class-j)
                     (class-j-given-node-probability node class-j classes priors class-counts)))))
         classes))


(defn tree-complexity
  "A tree's complexity is defined as the
  number of its terminal nodes"
  [tree]
  (if (and (nil? (:left tree)) (nil? (:right tree)))
    1
    (+ (tree-complexity (:left tree))
       (tree-complexity (:right tree)))))


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
   misclassification-cost
   class-counts]
  (sum
    (for [node (get-terminal-nodes tree [])]
      (* (node-cost node classes priors misclassification-cost class-counts)  ;; r(t)
         (fall-into-node-probability node classes priors class-counts)))))  ;; p(t)


(defn pruning-objective
  "The \"Minimum Cost Complexity\" Tree Objective as defined in CART"
  [tree
   alpha
   classes
   priors
   misclassification-cost
   class-counts]
  (+ ;; R(T)
    (tree-cost tree classes priors misclassification-cost class-counts)
    ;; a*|~T|
    (* alpha (tree-complexity tree))))


(defn accuracy
  "Compute accuracy"
  [truth predictions]
  (float (/ (sum (map #(if (= %1 %2) 1 0) truth predictions))
            (count truth))))



(defn resubstitution-error
  [class-counts]
  (let [vs (vals class-counts)]
    (- 1 (/ (apply max vs) (sum vs)))))



;; reserve small amount of probability mass for events
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