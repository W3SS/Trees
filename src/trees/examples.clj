(ns trees.examples
  (:require [trees.dataframe :as df]))


(def sample-data
  [[:age :married? :own-house? :income :gender :class]
   [22 :no :no 28000 :male :bad]
   [46 :no :yes 32000 :female :bad]
   [24 :yes :yes 24000 :male :bad]
   [25 :no :no    27000 :male :bad]
   [29 :yes :yes  32000 :female :bad]
   [45 :yes :yes  30000 :female :good]
   [63 :yes :yes  58000 :male :good]
   [36 :yes :no   52000 :male :good]
   [23 :no :yes   40000 :female :good]
   [50 :yes :yes  28000 :female :good]])


(def rich-example {:age 50 :married? true :own-house? true :income 200000 :gender :male})
(def poor-example {:age 50 :married? true :own-house? false :income 25000 :gender :male})
(def notes-example {:age 42 :married? false :own-house? true :income 30000 :gender :male})


(defn yn-bool
  [x]
  (cond (= x :no) false
        (= x :yes) true))


(defn parse-float
  [x]
  (Float/parseFloat x))


(defn load-sample-data
  []
  (-> (df/from-tabular (first sample-data) (rest sample-data) {})
      (df/typify-attribute-df :age :int :numerical identity)
      (df/typify-attribute-df :married? :boolean :categorical yn-bool)
      (df/typify-attribute-df :own-house? :boolean :categorical yn-bool)
      (df/typify-attribute-df :income :int :numerical identity)
      (df/typify-attribute-df :gender :enum :categorical identity)
      (df/typify-attribute-df :class :enum :categorical identity)))



(defn load-iris-data
  []
  (-> (df/from-csv "resources/data/iris.tsv" \tab)
      (df/typify-attribute-df "Petal width" :float :numerical parse-float)
      (df/typify-attribute-df "Sepal width" :float :numerical parse-float)
      (df/typify-attribute-df "Petal length" :float :numerical parse-float)
      (df/typify-attribute-df "Sepal length" :float :numerical parse-float)
      (df/typify-attribute-df "Species" :enum :categorical identity)))