(ns cliff.printer
  (:require [clojure.string :as string]))

(defn printable-argument [arg-type arg]
  (case arg-type
    :argument       arg
    :long-flag      (str "--" arg)
    :shorthand-flag (str "-" arg)))

(defn sentence [words]
  (if (= 1 (count words))
    (first words)
    (str (string/join ", " (butlast words)) " and " (last words))))
