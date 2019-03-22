(ns cliff.built-in
  (:require [clojure.java.io :as io]))

(defmulti parse-value (fn [attrs value]
                        (:type attrs)))

(def parsers-map
  {:boolean (constantly true)
   :double  #(Double/parseDouble %)
   :int     #(Integer/parseInt %)
   :file    io/file
   :keyword keyword
   :string  identity})

(defmethod parse-value :default [{:keys [type] :as attrs} value]
  (when-let [parser (get parsers-map type)]
    (parser value)))

(defn boolean? [spec]
  (= :boolean (:type spec)))
