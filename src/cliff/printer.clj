(ns cliff.printer
  (:require [clojure.string :as string]))

(defn keyword->arg-str [key]
  (->> key
       name
       (re-find #"^([^\?]+)\??$")
       last))

(defn argument [argkey]
  (keyword->arg-str argkey))

(defn long-flag [flag-key]
  (str "--" (keyword->arg-str flag-key)))

(defn sentence [words]
  (if (= 1 (count words))
    (first words)
    (str (string/join ", " (butlast words)) " and " (last words))))
