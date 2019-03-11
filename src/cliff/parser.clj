(ns cliff.parser
  (:require [cliff.built-in :as built-in]
            [clojure.string :as string]))

(defmulti tokenize :token-type)

(defmethod tokenize :shorthand-flag
  [_ input]
  (->> input
       rest
       (map str)
       (filter (partial not= "="))))

(def ^:private build-long-flag-tokenizer (memoize (fn [long-flags]
                                                    (->> (keys long-flags)
                                                         (string/join "|")
                                                         (format "^--(%s)=?(.*)$")
                                                         (re-pattern)))))

(defmethod tokenize :long-flag
  [{:keys [long-flags]} input]
  (remove string/blank? (rest (re-find (build-long-flag-tokenizer long-flags) input))))

(defmethod tokenize :argument
  [_ input]
  [input])

(defn- tokenize-next-arg [{:keys [args] :as context}]
  (let [[raw-token & args] args
        parsing-tokens     (tokenize context raw-token)]
    (assoc context
           :args args
           :parsing-tokens parsing-tokens
           :raw-token raw-token)))

(defn- next-token [{:keys [parsing-tokens] :as context}]
  (assoc context
         :current-token (first parsing-tokens)
         :parsing-tokens (next parsing-tokens)))

(defn- next-value [{:keys [parsing-tokens args] :as context} attributes]
  (cond
    (built-in/boolean? attributes) context
    (seq parsing-tokens)      (assoc context :current-value (apply str parsing-tokens) :parsing-tokens [])
    :else                     (assoc context :current-value (first args) :args (next args))))

(defn- printable-token-type [token-type]
  (string/replace (name token-type) #"-" " "))

(defn- unknown-token [{:keys [current-token raw-token token-type]}]
  {:status  :error
   :message (format "Unknown %s: '%s' in %s." (printable-token-type token-type) current-token raw-token)})

(defn- unparseable-value [{:keys [current-token raw-token token-type]} value]
  {:status  :error
   :message (format "Unparseable value: %s for %s '%s' in %s." value (printable-token-type token-type) current-token raw-token)})

(defn- parse-value [{:keys [current-value] :as context} {:keys [name] :as attributes}]
  (try
    (assoc-in context [:result name] (built-in/parse-value attributes current-value))
    (catch Exception e
      (unparseable-value context current-value))))

(defn- parse-flag [{:keys [current-token long-flags shorthand-flags] :as context}]
  (if-let [attributes (get long-flags current-token
                        (get shorthand-flags current-token))]
    (parse-value (next-value context attributes) attributes)
    (unknown-token context)))

(defn- parse-shorthand-flags [context]
  (loop [{:keys [status parsing-tokens] :as context}
         (parse-flag context)]
    (if (or (= status :error)
            (not (seq parsing-tokens)))
      context
      (recur (parse-flag (next-token context))))))

(defn- parse-positional-argument [{:keys [current-arg-position current-token] :as context
                                   :or   {current-arg-position 0}}]
  (if-let [attributes (get-in context [:arguments current-arg-position])]
    (update (parse-value (assoc context :current-value current-token) attributes)
            :current-arg-position (fnil inc 0))
    (unknown-token context)))

(defn- parse-token [{:keys [token-type] :as context}]
  (case token-type
    :shorthand-flag (parse-shorthand-flags context)
    :long-flag      (parse-flag context)
    :argument       (parse-positional-argument context)))

(def shorthand-flag? (partial re-find #"^-[^\-]+"))

(def long-flag? #(string/starts-with? % "--"))

(defn- parse-next-arg [{[arg] :args :as context}]
  (let [token-type
        (cond
          (shorthand-flag? arg) :shorthand-flag
          (long-flag? arg)      :long-flag
          :else                 :argument)]
    (-> context
        (assoc :token-type token-type)
        tokenize-next-arg
        next-token
        parse-token)))

(defn- continue-parsing? [{:keys [status args]}]
  (and (not= status :error)
       (seq args)))

(defn- parse-args [context]
  (loop [context context]
    (if-not (continue-parsing? context)
      context
      (recur (parse-next-arg context)))))

(defn- keyword->long-flag [key]
  (->> key
       name
       (re-find #"^([^\?]+)\??$")
       last))

(defn parser-context [{:keys [arguments flags]} args]
  (letfn [(get-arguments []
            (->> arguments
                 (map-indexed (partial vector))
                 (reduce #(merge %1 %2) {})))
          (get-flags []
                     (->> flags
                          (map (fn [[name {:keys [shorthand] :as flag}]]
                                 [(keyword->long-flag name) shorthand (assoc flag :name name)]))
                          (reduce (fn [result [long-flag shorthand-flag flag-attrs]]
                                    (-> result
                                        (assoc-in [:long-flags long-flag] flag-attrs)
                                        (assoc-in [:shorthand-flags shorthand-flag] flag-attrs)))
                                  {})))]
    (assoc (get-flags)
           :arguments (get-arguments)
           :args args)))

(defn parse [program args]
  (let [context                     (parser-context program args)
        {:keys [status] :as output} (parse-args context)]
    (-> output
        (cond-> (nil? status) (assoc :status :ok))
        (select-keys [:status :message :result]))))
