(ns cart.util)


(defmacro spy
  [expr]
  `(let [foo# ~expr]
     (println "TYPE" (type foo#))
     (println "VALUE" foo#)
     foo#))