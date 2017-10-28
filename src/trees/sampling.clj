(ns trees.sampling
  (:require [trees.dataframe :as df])
  (:import (java.util.concurrent ThreadLocalRandom)))


(defn generate-bootstrap-sample
  "Samples with replacement to create a data-frame of size `sample-size`"
  [df sample-size]
  (let [bound   (int (df/df-count df))
        indices (repeatedly sample-size #(. (ThreadLocalRandom/current) (nextInt bound)))]
    (df/select-by-indices-df df indices)))


(defn train-test-split
  "Shuffles data, splits df into two based on test reserve size"
  [df test-reserve]
  (let [N             (df/df-count df)
        indices       (shuffle (range N))
        test-size     (long (* N test-reserve))]
    [(df/select-by-indices-df df (drop test-size indices))
     (df/select-by-indices-df df (take test-size indices))]))