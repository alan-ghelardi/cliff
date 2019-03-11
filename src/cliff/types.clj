(ns cliff.types)

(defmulti parse-value :type)

(defmethod parse-value :boolean [_ _]
  true)

(defmethod parse-value :int [_ value]
  (Integer/parseInt value))

(defmethod parse-value :default [_ value]
  value)

(defn boollean? [spec]
  (= :boolean (:type spec)))
