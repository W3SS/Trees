(ns cart.conversions)


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