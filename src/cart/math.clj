(ns cart.math
  (:refer-clojure :exclude [max min]))


(defn find-maximal-values
  [f xs]
  (let [x* (first xs)
        y* (f x*)]
    (reduce (fn [acc x]
              (let [[arg val] acc
                    y (f x)]
                (if (> y val)
                  [x y]
                  acc)))
            [x* y*]
            (rest xs))))


(defn find-minimal-values
  [f xs]
  (let [x* (first xs)
        y* (f x*)]
    (reduce (fn [acc x]
              (let [[arg val] acc
                    y (f x)]
                (if (< y val)
                  [x y]
                  acc)))
            [x* y*]
            (rest xs))))


(defn max
  [f xs]
  (second (find-maximal-values f xs)))


(defn min
  [f xs]
  (second (find-minimal-values f xs)))


(defn arg-max
  [f xs]
  (first (find-maximal-values f xs)))


(defn arg-min
  [f xs]
  (first (find-minimal-values f xs)))