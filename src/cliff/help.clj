(ns cliff.help
  (:require [cliff.built-in :as built-in]
            [clojure.string :as string]))

(defn argument-name [arg]
  (->> arg
       name
       (re-find #"^([^\?]+)\??$")
       last))

(defn long-flag [flag-name]
  (str "--" (argument-name flag-name)))

(defn printable-str [x]
  (if (keyword? x)
    (name x)
    (str x)))

(defn sentence
  ([words] (sentence words {}))
  ([words options]
   (let [{:keys [sort? quote?]} options
         words                  (-> (map printable-str words)
                                    (cond->> sort? sort
                                             quote? (map #(str "'" % "'"))))]
     (if (= 1 (count words))
       (first words)
       (str (string/join ", " (butlast words)) " and " (last words))))))

(defn- describe [{:keys [default help value-name values]}]
  (str help
       (when values
         (format ". %s can be %s" value-name (sentence values {:sort? true :quote? true})))
       (when default
         (format " (default %s)" (printable-str default)))))

(defmacro or-empty-str [predicate & forms]
  `(if-not ~predicate
     ""
     (do ~@forms)))

(defn- flag-usage [[flag-name {:keys [list? shorthand type value-name] :as arg-spec}]]
  (let [value-name (when-not (built-in/boolean? arg-spec)
                     (string/upper-case (or value-name (argument-name flag-name))))]
    [(or-empty-str shorthand
                   (str "-" shorthand ","))
     (long-flag flag-name)
     (or-empty-str value-name
                   value-name)
     (or-empty-str (not (built-in/boolean? arg-spec))
                   (if list?
                     (str "list[" (name type) "]")
                     (name type)))
     (describe (assoc arg-spec :value-name value-name))]))

(defn flags-usage [flags]
  (when flags
      (cons "Options"
            (sort-by second
                     (map flag-usage flags)))))
