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

(defn- printable-arg-value [flag-name {:keys [value-name values] :as arg-spec}]
  (when-not (built-in/boolean? arg-spec)
    (string/upper-case (or value-name (argument-name flag-name)))))

(defn- flag-usage [[flag-name {:keys [list? shorthand type value-name] :as arg-spec}]]
  (let [value-name (printable-arg-value flag-name arg-spec)]
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

(defn- show-requiredness [arg required?]
  (if required?
    (str "<" arg ">")
    (str "[" arg "]")))

(defn- list-arguments [arguments]
  (map (fn [{:keys [name list? required?] :as arg-spec}]
         (let [arg-name (string/upper-case (argument-name name))]
           (show-requiredness (if list?
                                (str "& " arg-name)
                                arg-name)
                              required?)))
       arguments))

(defn- list-flags [flags]
  (letfn [(required-flag [flag-name {:keys [required?] :as flag-spec}]
            (show-requiredness (str (long-flag flag-name) " " (printable-arg-value flag-name flag-spec)) required?))]
    (reduce (fn [[head :as result] [flag-name {:keys [required?] :as flag-spec}]]
              (cond
                required?               (conj result (required-flag flag-name flag-spec))
                (not= head "[OPTIONS]") (cons "[OPTIONS]" result)
                :else                   result))
            [] flags)))

(defn command-overview [{:keys [path arguments flags]}]
  (as-> (list-flags flags) result
    (into path result)
    (into result (list-arguments arguments))
    (string/join " " result)))
