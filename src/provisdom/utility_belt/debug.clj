(ns provisdom.utility-belt.debug)

(defmacro dbg
  "Wrap an expression with (dbg expression) to see it print out during
  debugging."
  [x]
  `(let [x# ~x]
     (println "dbg:" '~x "=" x#)
     x#))