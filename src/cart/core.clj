(ns cart.core)

(defprotocol Model
  (learn [] "")
  )
(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
