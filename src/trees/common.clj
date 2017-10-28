(ns trees.common
  (:require [trees.math :refer [sum]]))


(defn terminal?
  [node]
  (and (seq node)
       (nil? (:left node))
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


(defn total-number
  [root]
  (sum (vals (:class-counts root))))


(defn satisfies-pred?
  "Returns the indices of values which matched the predicate"
  [p xs]
  (keep-indexed (fn [i v] (when (p v) i)) xs))


(defn get-majority-class
  "Finds the key with highest value in the map"
  [counts]
  (first (apply max-key second counts)))