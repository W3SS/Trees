(ns trees.splitting
  (:require [trees.dataframe :as df]))


(defn satisfies-pred?
  "Returns the indices of values which matched the predicate"
  [p xs]
  (keep-indexed (fn [i v] (when (p v) i)) xs))


(defn score-potential-threshold
  [data
   attribute-values
   root-impurity
   impurity-measure
   potential-threshold]
  (let [left-indices    (satisfies-pred? #(< potential-threshold %) attribute-values)
        left-histogram  (frequencies (df/select-by-indices data left-indices))
        right-indices   (satisfies-pred? #(>= potential-threshold %) attribute-values)
        right-histogram (frequencies (df/select-by-indices data right-indices))

        ;; FIXME: this score may not be totally correct
        ;; TODO: look into this calculation
        score           (- root-impurity (impurity-measure left-histogram) (impurity-measure right-histogram))]
    [score potential-threshold]))


(defn split-numerical
  "returns the impurity reduction"
  [data attribute impurity-measure root-impurity]
  (let [attribute-values (df/get-attribute-values data attribute)]
    (->> attribute-values
         (distinct)
         (sort)
         (map (partial score-potential-threshold data attribute-values root-impurity impurity-measure))
         (sort-by first)
         (first))))