(ns trees.random
  (:require [trees.dataframe :as df]
            [clojure.math.combinatorics :refer [combinations]])
  (:import (java.util.concurrent ThreadLocalRandom)))


(defn rand-int-between
  [a b]
  (. (ThreadLocalRandom/current) (nextInt (int a) (int b))))


(defn rand-ints-between
  [n a b]
  (repeatedly n #(rand-int-between a b)))


;; TODO: add an assertion here?
(defn rand-float-between
  "Rand float between a and b"
  [a b]
  (-> (ThreadLocalRandom/current)
      (.nextFloat)
      (* (- b a))
      (+ a)))


(defn rand-floats-between
  [n a b]
  (repeatedly n #(rand-float-between a b)))



(defn select-random-feature-subset
  "Choose a size K subset of feature to split on"
  [features K]
  (take K (shuffle features)))


(defn enumerated-zip2
  [xs ys]
  (map vector (range) xs ys))


(defn dot
  [xs ys]
  (reduce + (map * xs ys)))


(defn generate-pseudo-features
  "Generates K psuedo-features which are linear
  combinations of a size L random subset of all
  existing features."
  [data features K L]
  (let [feature-subsets  (repeatedly K #(take L (shuffle features)))
        subset-weights   (repeatedly K #(rand-floats-between L -1 1))
        to-values        (partial df/get-attribute-values data)]
    (for [[i subset weights] (enumerated-zip2 feature-subsets subset-weights)]
      [(str "PF-" i) (map #(dot weights %) (apply map vector (map to-values subset)))])))