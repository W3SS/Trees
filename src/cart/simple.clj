(ns cart.simple
  "Simple proof-of-concept of decision tree learning in pure Clojure"
  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io])
  (:import (clojure.lang ILookup)))


;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;                    DataFrame Utilities
;;
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


(defn get-attribute-values
  [data feature]
  (get-in data [feature :values]))

(defn get-attribute-storage-type
  [data feature]
  (get-in data [feature :storage-type]))

(def reserved-keys #{:df/count :df/source :df/source-type})

(defn keep-attributes
  [df features]
  (select-keys df (concat reserved-keys features)))

(defn nth-row
  [data-frame n]
  (into {}
        (for [[k m] data-frame
              :when (not (reserved-keys k))
              ]
          [k (nth (:values m) n)])))


;; NOTE: we have a tough time enforcing uniformity in data-length here. Have to rely on invariants at construction time.
(def example-data-frame
  {"A" {:values [1 2 3]
        :storage-type :int
        :domain-type :discrete}
   "B" {:values [5.0 6.0 7.0]
        :storage-type :float
        :domain-type :continuous}
   })

(defmacro spy
  [expr]
  `(let [foo# ~expr]
     (println "TYPE" (type foo#))
     (println "VALUE" foo#)
    foo#))


(defn count-pos-and-neg
  [xs]
  (reduce (fn [[pos neg] x]
            (if (= 1 x)
              [(inc pos) neg]
              [pos (inc neg)]))
          [0 0]
          xs))


(defn node-misclassified
  "Majority wins"
  [labels-in-node]
  (if (empty? labels-in-node)
    (long 0)
    (long (apply min (count-pos-and-neg labels-in-node)))))


;; TODO: implement Counted protocol
(defn df-count
  [df]
  (:df/count df))


(comment
  (def ex1 [-1, -1, 1, 1, 1])
  (def ex2 [-1, -1, 1, 1, 1, 1, 1])
  (def ex3 [-1, -1, -1, -1, -1, 1, 1])

  (assert (= (node-misclassified ex1) 2))
  (assert (= (node-misclassified ex2) 2))
  (assert (= node-misclassified ex3) 2))


(defn satisfies-pred?
  [p xs]
  (keep-indexed (fn [i v] (when (p v) i)) xs))


(defn select-by-indices
  [xs is]
  (persistent!
    (reduce (fn [acc i] (conj! acc (nth xs i)))  ;; NOTE: we should do this in array-based fashion (one lane at a time).
            (transient [])
            is)))

;; assumes our ghetto data-frame structure
;; TODO: preserve count and shit
(defn select-by-indices-df
  [df is]
  ;;(println "GIVEN INDICES: " is)
  (assoc
  (into {}
        (for [[k m] df
              :when (not (reserved-keys k))]
          [k {:values (select-by-indices (:values m) is)
              :storage-type (:storage-type m)
              :domain-type (:domain-type m)}]))
    :df/count (count is)
    :df/source (:df/source df)
    :df/source-type (:df/source-type df)))


;; hmm...csv seems to already do this.
(defn infer-value-types
  [vs])


;; TODO: add ability to specify header...
;; TODO: support for unboxed collections
(defn csv->df
  ""
  [file]
  (with-open [rdr (io/reader file)]
    (let [raw-csv       (csv/parse-csv rdr)
          header        (first raw-csv)
          body          (rest raw-csv)
          short-buffers (vec (repeat (count header) []))
          row-count     (atom 0)
          extended-buffers (persistent!
                             (reduce (fn [bufs row]
                                       (swap! row-count inc)
                                       (loop [bufs' bufs
                                              vs row
                                              i 0]
                                         (if (seq vs)
                                           (recur (assoc! bufs' i (conj (nth bufs' i) (first vs)))
                                                  (rest vs)
                                                  (inc i))
                                           bufs')))
                                     (transient short-buffers)
                                     body))]
      (assoc (into {} (map (fn [k vs] {k {:storage-type :string
                                          :domain-type :unknown
                                          :values vs}})
                           header
                           extended-buffers))
        :df/count @row-count
        :df/source file
        :df/source-type :csv))))


(defn typify-attribute-df
  [df attribute stype dtype f]
  (-> df
      (update-in [attribute :values] (fn [xs] (mapv f xs)))
      (assoc-in [attribute :storage-type] stype)
      (assoc-in [attribute :domain-type] dtype)))


(defn str->int
  [x]
  (Integer/parseInt x))

(defn str->float
  [x]
  (Float/parseFloat x))

(defn str->double
  [x]
  (Double/parseDouble x))

(defn str->long
  [x]
  (Long/parseLong x))

(defn enumerate
  [xs]
  (map vector xs (range)))


(defn enumize-attribute
  "Maps an implicit enum to integers"
  [df attribute]
  (let [mapping (-> (get-attribute-values df attribute)
                    (set)
                    (sort)
                    (enumerate)
                    (->> (into {})))]
    (-> df
      (update-in [attribute :values] (fn [xs] (map mapping xs)))
      (assoc :storage-type :int
             :domain-type :categorical
             :mapping mapping))))

(defn enumize-attributes
  [df attributes]
  (reduce enumize-attribute df attributes))


(defn attribute-value->feature
  [attribute value]
  (str attribute "_" value))


(defn binary-encode-attribute
  [df attribute]
  (let [all-values          (get-attribute-values df attribute)
        new-attributes      (for [v (distinct all-values)] (attribute-value->feature attribute v))]
    (reduce (fn [df' new-attr]
              (let [binary-encoding (mapv #(if (= (attribute-value->feature attribute %) new-attr)
                                             1
                                             0)
                                          all-values)]
                (assoc df' new-attr {:values binary-encoding
                                     :storage-type :int
                                     :domain-type :binary})))
            df
            new-attributes)))


(defn binary-encode-attributes
  "Takes categorical attributes and binary encodes them"
  [df attributes]
  (reduce binary-encode-attribute df attributes))

(defn train-test-split
  "Shuffles data, splits df into two based on test reserve size"
  [df test-reserve]
  (let [N             (df-count df)
        indices       (shuffle (range N))

        test-size     (long (* N test-reserve))
        ]
    [(select-by-indices-df df (drop test-size indices))
     (select-by-indices-df df (take test-size indices))]))

;; TODO: cross-validation

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;                    Decision Tree Stuff
;;
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defn find-best-splitting-feature
  [data features target]
  (let [target-values   (get-attribute-values data target)
        num-data-points (float (df-count data))]
    ;; Loop through each feature to consider splitting on that feature
    (reduce (fn [acc feature]
              (let [;;_ (println "CHOSEN FEATURE" feature)
                    best-error      (:error acc)
                    best-feature    (:feature acc)
                    fvs             (get-attribute-values data feature)
                    left-split      (select-by-indices-df data (satisfies-pred? zero? fvs))
                    right-split     (select-by-indices-df data (satisfies-pred? #(= % 1) fvs))
                    A (get-attribute-values left-split target)
                    B (get-attribute-values right-split target)
                    left-mistakes   (node-misclassified A)
                    right-mistakes  (node-misclassified B)
                    error           (/ (+ left-mistakes right-mistakes) num-data-points)]
                (if (< error best-error)
                  {:error error :feature feature}
                  acc)))
            {:error 2.0 :feature nil}
            features)))



(defn create-leaf
  [target-values]
  ;; Create a leaf node
  (let [[num-pos num-neg] (count-pos-and-neg target-values)]
    (if (> num-pos num-neg)
      {:prediction :pos :leaf? true}
      {:prediction :neg :leaf? true})))


(defn decision-tree-create
  ([data features target] (decision-tree-create data features target 0 10))
  ([data features target current-depth max_depth]

    ;; Stopping condition 1:
    ;;   Check if there are mistakes at current node.
    ;; Stopping condition 2 (check if there are remaining features to consider splitting on)
    ;;   If there are no remaining features to consider, make current node a leaf node
    ;; Stopping condition 3:
    ;;   Reached maximum depth

   (let [;_ (println "DF: " data)
         target-values (get-attribute-values data target)
         _ (println "--------------------------------------------------------------------")
         _ (println (format "Subtree, depth = %s (%s data points)." current-depth (count target-values)))]
     (cond (zero? (node-misclassified target-values)) (do (println "Stopping condition 1 reached.")
                                                          ;; If not mistakes at current node, make current node a leaf node
                                                          (create-leaf target-values))

           (empty? features) (do (println "Stopping condition 2 reached.")
                                 (create-leaf target-values))

           (>= current-depth max_depth) (do (println "Reached maximum depth. Stopping for now.")
                                            (create-leaf target-values))

           :default (let [result (find-best-splitting-feature data features target)
                          splitting-feature (:feature result)
                          splitting-error (:error result)
                          fvs                 (get-attribute-values data splitting-feature)
                          left-split          (select-by-indices-df data (satisfies-pred? zero? fvs))
                          right-split         (select-by-indices-df data (satisfies-pred? #(= % 1) fvs))
                          remaining-features  (disj features splitting-feature)]

                      (println (format "Split on feature %s. (%s, %s)" splitting-feature (df-count left-split) (df-count right-split)))

                      ;; Create a leaf node if the split is "perfect"
                      (cond (= (df-count left-split) (df-count data))   (do
                                                                          (println "Creating leaf node.")
                                                                          (create-leaf (get-attribute-values left-split target)))
                            (= (df-count right-split) (df-count data))  (do
                                                                          (println "Creating leaf node.")
                                                                          (create-leaf (get-attribute-values right-split target)))
                            :default (let [left-tree  (decision-tree-create left-split remaining-features target (inc current-depth) max_depth)
                                           right-tree (decision-tree-create right-split remaining-features target (inc current-depth) max_depth)]
                                       {:leaf? false
                                        :left left-tree
                                        :right right-tree
                                        :splitting-feature splitting-feature})))))))


(defn classify
  "Given a fully constructed decision tree T and
  a record X, return the predicted label"
  [tree x]
  (if (:leaf? tree)
    (:prediction tree)
    (let [split_feature_value (get x (:splitting-feature tree))]
      (if (zero? split_feature_value)
        (recur (:left tree) x)
        (recur (:right tree) x)))))


(defn drop-attributes
  "Removes fields from dataframe"
  [df attributes]
  (apply dissoc df attributes))