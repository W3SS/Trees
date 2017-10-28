(ns trees.regression
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            [trees.common :refer :all]
            [trees.dataframe :as df]
            [trees.math :refer [sum]]
            [trees.measures :as m]
            [trees.splitting :refer [calculate-regression-split]]
            [taoensso.timbre :as log]))

;; WARN: temporary copy-paste of core until we abstract out the shared logic


(defn find-best-splitting-feature
  "Searches across all features to find the one with lowest error"
  [data features target parent-impurity]
  (log/debug "SEARCHING FOR BEST SPLITTING FEATURE")
  (log/debug "FEATURES: " features)
  (log/debug "TARGET: " target)
  ;; TODO: why get these a second time? Already calculated them before
  (let [target-values   (df/get-attribute-values data target)
        num-data-points (float (df/df-count data))]
    (log/debug "RESPONSE: " target-values)
    (log/debug "NUM DATA POINTS: " num-data-points)
    ;; Loop through each feature to consider splitting on that feature
    (reduce (fn [acc feature]
              (let [best-quality    (or (:quality acc) -10000)  ;; FIXME: figure out a better way to do this
                    feature-type    (df/get-attribute-domain-type data feature)
                    [quality value] (calculate-regression-split feature-type data feature target parent-impurity)]
                (when (nil? quality)
                  (log/debug "NIL QUALITY ON ATTR:" feature)
                  (log/debug "GIVEN VALUES: " (df/get-attribute-values data feature))
                  )
                (log/debug "QUALITY: " quality)
                (log/debug "BEST QUALITY: " best-quality)
                (cond (nil? quality) acc
                      (> quality best-quality) {:quality quality
                                                :feature feature
                                                :feature-type feature-type
                                                :value value}
                      :default acc)))
            {}
            features)))


(defn create-leaf
  "Given a mapping from class -> count in node,
  creates a tree leaf node"
  [mean answer]
  {:prediction mean
   :leaf? true
   :answer answer})


(defn feature-type-predicate
  [value feature-type]
  (case feature-type
    :numerical #(<= % value)
    (throw (ex-info "Unsupported feature type" {:type feature-type}))))


(defn pure?
  "Determines if only a single class is within a node"
  [response-counts]
  (= (count (keys response-counts)) 1))


(defn log-message
  [target-values current-depth]
  (do
    (log/debug "TARGET VALUES:" target-values)
    (log/debug "--------------------------------------------------------------------")
    (log/debug (format "Subtree, depth = %s (%s data points)." current-depth (count target-values)))))


(defn learn
  ([data features target] (learn data features target 5 0 10))
  ([data features target min-count current-depth max_depth]
   (df/validate! data)
   (when (not (set? features))
     (throw (IllegalArgumentException. "features must be a set")))
    ;; Stopping condition 1:
    ;;   Check if there are mistakes at current node.
    ;; Stopping condition 2 (check if there are remaining features to consider splitting on)
    ;;   If there are no remaining features to consider, make current node a leaf node
    ;; Stopping condition 3:
    ;;   Reached maximum depth

   (let [target-values    (df/get-attribute-values data target)
         _ (log-message target-values current-depth)
         response-counts  (frequencies target-values)
         mean-response    (m/mean target-values)]
     (log/debug "response COUNTS: " response-counts)
     (cond (pure? response-counts) (do (log/debug "Stopping condition 1 reached.")
                                    ;; If not mistakes at current node, make current node a leaf node
                                    (create-leaf mean-response :early-stop))

           ;; NOTE: do we do nothing if the min-count is hit?!??!!?!
           (<= (count target-values) min-count) (do (log/debug "Stopping condition 2 reached.")
                                                    ;; If not mistakes at current node, make current node a leaf node
                                                    (create-leaf mean-response :early-stop))

           ;(empty? features) (do (log/debug "Stopping condition 3 reached.")
           ;                      (create-leaf mean-response :early-stop))

           (>= current-depth max_depth) (do (log/debug "Reached maximum depth. Stopping for now.")
                                            (create-leaf mean-response :early-stop))

           :default (let [_ (log/debug "FELL THROUGH ALL")
                          parent-impurity     (m/sum-of-squared-deviations* target-values mean-response)
                          result              (find-best-splitting-feature data features target parent-impurity)
                          _ (log/debug "BEST:" result)
                          splitting-feature   (:feature result)
                          splitting-value     (:value result)
                          splitting-type      (:feature-type result)
                          splitting-quality   (:quality result)

                          splitting-pred      (feature-type-predicate splitting-value splitting-type)

                          fvs                 (df/get-attribute-values data splitting-feature)
                          left-split          (df/select-by-indices-df data (satisfies-pred? splitting-pred fvs))
                          right-split         (df/select-by-indices-df data (satisfies-pred? (complement splitting-pred) fvs))
                          remaining-features  features ;; (disj features splitting-feature)
                          ]

                      (log/debug (format "Split on feature %s. (%s, %s)" splitting-feature (df/df-count left-split) (df/df-count right-split)))

                      ;; Create a leaf node if the split is "perfect"
                      (cond (= (df/df-count left-split) (df/df-count data))   (do
                                                                                (log/debug "Creating leaf node.")
                                                                                (create-leaf (m/mean (df/get-attribute-values left-split target))
                                                                                             :yes))
                            (= (df/df-count right-split) (df/df-count data))  (do
                                                                                (log/debug "Creating leaf node.")
                                                                                (create-leaf (m/mean (df/get-attribute-values right-split target))
                                                                                             :no))
                            :default (let [left-tree  (learn left-split remaining-features target min-count (inc current-depth) max_depth)
                                           right-tree (learn right-split remaining-features target min-count (inc current-depth) max_depth)]
                                       {:leaf? false
                                        :left (assoc left-tree :answer :yes)
                                        :right (assoc right-tree :answer :no)
                                        :splitting-feature splitting-feature
                                        :splitting-value splitting-value
                                        :splitting-type splitting-type})))))))


(defn predict
  "Given a fully constructed decision tree T and
  a record X, return the predicted label"
  [tree x]
  (if (:leaf? tree)
    (:prediction tree)
    (let [splitting-feature (:splitting-feature tree)
          observed-value    (get x splitting-feature)
          splitting-value   (:splitting-value tree)
          splitting-type    (:splitting-type tree)
          predicate         (feature-type-predicate splitting-value splitting-type)]
      (log/debug "SPLITTING ON:" splitting-feature splitting-type)
      (log/debug "OBSERVED:" observed-value)
      (when (= splitting-type :numerical)
        (log/debug observed-value "<=" splitting-value "?" (predicate observed-value)))
      (when (= splitting-type :categorical)
        (log/debug observed-value "in" splitting-value "?" (predicate observed-value)))
      (if (predicate observed-value)
        (recur (:left tree) x)
        (recur (:right tree) x)))))



