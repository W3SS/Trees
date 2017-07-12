(ns cart.model)

(def tree-node
  {
   :type         ;; :leaf, :branch, :cut, :subset
   :class        ;; most frequent class at this node
   :items        ;; number of items at this node
   :distribution ;; class distribution of items
   :errors       ;; number of errors at this node
   :attribute    ;; attribute referenced in test
   :forks        ;; number of branches at this node
   :cut          ;; threshold for continuous attribute
   :lower        ;; lower limit of soft threshold
   :upper        ;; upper limit of soft threshold
   :subset       ;; subsets of discrete values
   :branches     ;; branches[i] = (sub)tree for outcome x
})


(def attribute-test
  {
   :type         ;; node test type (see tree node)
   :attribute    ;; attribute tested
   :forks        ;; possible branches
   :cut		       ;; threshold (if relevant)
   :subset       ;; subset (if relevant)
   })

(def outcome
  {:test
   :outcome
   })


(def rule
  {
   :condition-count ;; number of conditions
   :conditions      ;; the rule conditions
   :class           ;; class given by rule
   :error           ;; estimated error rate
   :bits            ;; bits to encode rule
   :used            ;; times rule used      ;; FIXME this might go away...
   :incorrect       ;; times rule incorrect
   })


(def rule-set
  {
   :rules           ;; rules
   :rules-count     ;; number of rules
   :ranking         ;; ranking of rules
   :default-class   ;; default class for this ruleset
   })