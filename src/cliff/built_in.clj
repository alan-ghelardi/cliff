(ns cliff.built-in)

(defmulti parse-value :type)

(defmethod parse-value :boolean [_ _]
  true)

(defmethod parse-value :int [_ value]
  (Integer/parseInt value))

(defmethod parse-value :keyword [_ value]
  (keyword value))

(defmethod parse-value :string [_ value]
  value)

(defn boolean? [spec]
  (= :boolean (:type spec)))
