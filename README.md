# Trees 

Classification And Regression Trees in Clojure

![image of olive tree](olive.png)

 
## Overview 

🚧 __WARNING: THIS SOFTWARE IS STILL PRE-ALPHA__ 🚧 

This library supports the construction and interpretation of decision trees
and their ensembles, along with utilities for visualizing trees and generating summaries of data. 


## Example Usage

```clj
(require '[trees.dataframe :as df]
         '[trees.tree :as t]
         '[trees.examples :refer [load-iris-data]]
         '[trees.measures :refer [accuracy]])

(def iris-data (load-iris-data))

(def split (t/train-test-split iris-data 0.20))
(def training-set (first split))
(def test-set (df/df->maps (second split)))

(def features #{"Sepal length" "Sepal width" "Petal length" "Petal width"})
(def target "Species")

(def iris-tree (t/learn training-set features target))


(def truth (map #(get % "Species") test-set))
(def predicted (map (partial t/classify iris-tree) test-set))

(def total (count truth))

(accuracy truth predicted)

;; To view the tree:
(require '[com.walmartlabs.datascope :as ds])
(ds/view iris-tree)
```

## FAQ

### What are decision trees?

They are a class of nonparametric model suitable for both classification and regression.

### Why should I use decision trees instead of some other method?

They are interpretable by design and can be very accurate. [Scikit](http://scikit-learn.org/stable/modules/tree.html) gives
a list of good reasons to use them.

## References

Directly based on the ideas and work laid out in:

  * Breiman, Leo, et al. Classification and regression trees. CRC press, 1984.
  * Breiman, Leo. "Random forests." Machine learning 45.1 (2001): 5-32.
  * Quinlan, J. Ross. C4. 5: programs for machine learning. Elsevier, 2014.
  * Kass, Gordon V. "An exploratory technique for investigating large quantities of categorical data." Applied statistics (1980): 119-127.
  * Hawkins, Douglas M., and Gordon V. Kass. "Automatic interaction detection." Topics in applied multivariate analysis (1982): 269-302.
  * Freund, Yoav, and Robert E. Schapire. "A desicion-theoretic generalization of on-line learning and an application to boosting." European conference on computational learning theory. Springer, Berlin, Heidelberg, 1995.
  * Friedman, Jerome H. "Multivariate adaptive regression splines." The annals of statistics (1991): 1-67.
  * Friedman, Jerome H. "Greedy function approximation: a gradient boosting machine." Annals of statistics (2001): 1189-1232.


The drawing of the olive tree is by [Ulisse Aldrovandi](https://en.wikipedia.org/wiki/Ulisse_Aldrovandi).

## Copyright and Licensing 

Copyright © 2017 Arthur Maciejewicz

Distributed under the Apache License, Version 2
