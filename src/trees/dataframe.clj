(ns trees.dataframe
  (:require [clojure.java.io :as io]
            [clojure-csv.core :as csv]
            [clojure.pprint :as pprint]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [taoensso.timbre :as log]))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;                    DataFrame Utilities
;;
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defn get-attribute-values
  [data feature]
  ;;(log/debug "GET " feature)
  (get-in data [feature :values]))


(defn get-attribute-storage-type
  [data feature]
  (get-in data [feature :storage-type]))


(defn get-attribute-domain-type
  [data attribute]
  (get-in data [attribute :domain-type]))


(def reserved-keys #{:df/count :df/source :df/source-type})


(defn validate!
  [df]
  (when-not (map? df)
    (throw (ex-info "Not a map!" {:type (type df)})))

  (when (nil? (:df/count df))
    (throw (ex-info "Dataframe must have a count!" {})))

  (doseq [[k v] df
          :when (not (reserved-keys k))]
    (when-not (map? v)
      (throw (ex-info "Inner map was not a map!" {:type (type v)})))

    (when (or (nil? (:values v))
              (nil? (:storage-type v))
              (nil? (:domain-type v)))
      (throw (ex-info "required keys are missing!" {:keys (keys v)})))))



(defn keep-attributes
  [df features]
  (select-keys df (concat reserved-keys features)))


(defn drop-attributes
  "Removes fields from dataframe"
  [df attributes]
  (apply dissoc df attributes))


(defn nth-row
  [data-frame n]
  (into {}
        (for [[k m] data-frame
              :when (not (reserved-keys k))]
          [k (nth (:values m) n)])))


;; TODO: implement Counted protocol
(defn df-count
  [df]
  (:df/count df))


(defn df->maps
  [df]
  (map #(nth-row df %) (range (df-count df))))


(defn select-by-indices
  [xs is]
  (persistent!
    (reduce (fn [acc i] (conj! acc (nth xs i)))  ;; NOTE: we should do this in array-based fashion (one lane at a time).
            (transient [])
            is)))

;; TODO: preserve count and other metadata
(defn select-by-indices-df
  "Given a set of indices, we return a new dataframe
  with the selected values"
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



(defn typify-attribute-df
  [df attribute stype dtype f]
  (-> df
      (update-in [attribute :values] (fn [xs] (mapv f xs)))
      (assoc-in [attribute :storage-type] stype)
      (assoc-in [attribute :domain-type] dtype)))


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


(def converters
  {:int (fn [x] (Integer/parseInt x))
   :float (fn [x] (Float/parseFloat x))
   :double (fn [x] (Double/parseDouble x))
   :long (fn [x] (Long/parseLong x))
   :date (fn [x] (tf/parse (tf/formatters :date) x))})


(defn from-tabular
  [header body types]
  (let [short-buffers (vec (repeat (count header) []))
        row-count     (atom 0)
        extended-buffers (persistent! (reduce (fn [bufs row]
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
    (into {:df/count @row-count}
          (map (fn [k vs]
                 (if-let [specified-type (get types k)]
                   (let [converter (get converters specified-type)]
                     [k {:storage-type specified-type
                         :domain-type :unknown
                         :values (map converter vs)}])
                   [k {:storage-type :string
                       :domain-type :unknown
                       :values vs}]))
               header
               extended-buffers))))

      ;;:df/source file
      ;;:df/source-type :csv))


;; TODO: add ability to specify header...
;; TODO: support for unboxed collections
(defn from-csv
  ""
  ([file]
   (from-csv file \,))
  ([file delimiter]
  (with-open [rdr (io/reader file)]
    (let [raw-csv       (csv/parse-csv rdr :delimiter delimiter)
          header        (first raw-csv)
          body          (rest raw-csv)
          base-df       (from-tabular header body {})]
      (assoc base-df :df/source file
                     :df/source-type ::csv)))))


(defn df-keys
  [df]
  (remove (reserved-keys (keys df))))


(defn print-data-frame
  [df]
  (doseq [k (remove reserved-keys (keys df))]
    (print k " | "))
  (println "")
  (println "----------------------------------------------------")
  (dotimes [i (df-count df)]
    (doseq [v (vals (nth-row df i))]
      (print v " | "))
    (println "")))


;; NOTE: we have a tough time enforcing uniformity in data-length here. Have to rely on invariants at construction time.
(def example-data-frame
  {:df/count 3
   "A" {:values [1 2 3]
        :storage-type :int
        :domain-type :discrete}
   "B" {:values [5.0 6.0 7.0]
        :storage-type :float
        :domain-type :continuous}
   })