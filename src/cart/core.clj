(ns cart.core)


(defn arg-max
  [f xs]
  (let [x* (first xs)
        y* (f x*)]
    (reduce (fn [acc x]
              (let [[arg val] acc
                    y (f x)]
                (if (> y val)
                  [x y]
                  acc)))
            [x* y*]
            (rest xs))))

(defn arg-min
  [f xs]
  (let [x* (first xs)
        y* (f x*)]
    (reduce (fn [acc x]
              (let [[arg val] acc
                    y (f x)]
                (if (< y val)
                  [x y]
                  acc)))
            [x* y*]
            (rest xs))))


(def attribute-types
  #{:discrete :continuous})


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
