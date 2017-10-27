(ns trees.splitting
  (:require [trees.dataframe :as df]
            [trees.measures :as m]
            [taoensso.timbre :as log]))


(defn satisfies-pred?
  "Returns the indices of values which matched the predicate"
  [p xs]
  (keep-indexed (fn [i v] (when (p v) i)) xs))


;; NOTE: what about the boundary values? Most things are going to be above/below them...
(defn score-potential-threshold
  [attribute-values
   target-values
   impurity-measure
   root-impurity
   potential-threshold]
  (let [left-indices    (satisfies-pred? #(<= potential-threshold %) attribute-values)
        left-num        (count left-indices)
        left-histogram  (frequencies (df/select-by-indices target-values left-indices))

        right-indices   (satisfies-pred? #(> potential-threshold %) attribute-values)
        right-num       (count right-indices)
        right-histogram (frequencies (df/select-by-indices target-values right-indices))

        total-num       (+ left-num right-num)
        ;;_ (log/debug "TOTAL:" total-num)

        quality         (m/split-quality impurity-measure
                                         root-impurity
                                         (/ left-num total-num)
                                         left-histogram
                                         (/ right-num total-num)
                                         right-histogram)]
    [quality potential-threshold]))


(defn midpoint
  [x y]
  (/ (float (+ x y)) 2.0))


;; WARN: length must be >= 2
(defn compute-midpoints
  [xs]
  (map midpoint xs (rest xs)))


(defn calculate-numerical-split
  "returns the impurity reduction"
  [data attribute target impurity-measure root-impurity]
  (let [attribute-values  (df/get-attribute-values data attribute)
        target-values     (df/get-attribute-values data target)]
    (->> attribute-values
         (sort)
         (distinct)
         (compute-midpoints)
         (map (partial score-potential-threshold attribute-values target-values impurity-measure root-impurity))
         (sort-by first >)
         (first))))


;; TODO: not all of these HAVE to be evaluated
(defn powerset
  [xs]
  (reduce (fn [acc x]
            (->> acc
                 (map #(set (concat #{x} %)))
                 (concat acc)
                 set))
          #{#{}}
          xs))


;; FIXME: hack until I figure out smarter method
(defn compute-valid-subsets
  [xs]
  (case (count xs)
    2 [#{(first xs)}]
    3 [#{(first xs)} #{(second xs)} #{(last xs)}]
    (-> xs (powerset) (rest) (butlast))))





;; TODO: DRY This
;; NOTE: what about the boundary values? Most things are going to be above/below them...
(defn score-potential-subset
  [attribute-values
   target-values
   impurity-measure
   root-impurity
   potential-subset]
  (let [left-indices    (satisfies-pred? #(contains? potential-subset %) attribute-values)
        left-num        (count left-indices)
        left-histogram  (frequencies (df/select-by-indices target-values left-indices))

        right-indices   (satisfies-pred? #(not (contains? potential-subset %)) attribute-values)
        right-num       (count right-indices)
        right-histogram (frequencies (df/select-by-indices target-values right-indices))

        total-num       (+ left-num right-num)

        quality         (m/split-quality impurity-measure
                                         root-impurity
                                         (/ left-num total-num)
                                         left-histogram
                                         (/ right-num total-num)
                                         right-histogram)]
    [quality potential-subset]))


(defn calculate-categorical-split
  [data attribute target impurity-measure root-impurity]
  (let [attribute-values  (df/get-attribute-values data attribute)
        target-values     (df/get-attribute-values data target)]
    (->> attribute-values
         (distinct)
         (compute-valid-subsets)
         (map (partial score-potential-subset attribute-values target-values impurity-measure root-impurity))
         (sort-by first >)
         (first))))



(defn calculate-split
  [feature-type & args]
  (case feature-type
    :numerical    (apply calculate-numerical-split args)
    :categorical  (apply calculate-categorical-split args)
    (throw (ex-info "Not supported" {:type feature-type}))))