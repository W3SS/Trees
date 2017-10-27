(ns trees.sampling
  (:require [trees.dataframe :as df])
  (:import (java.util.concurrent ThreadLocalRandom)))


(defn generate-bootstrap-sample
  "Samples with replacement to create a data-frame of size `sample-size`"
  [df sample-size]
  (let [bound   (int (df/df-count df))
        indices (repeatedly sample-size #(. (ThreadLocalRandom/current) (nextInt bound)))]
    (df/select-by-indices-df df indices)))