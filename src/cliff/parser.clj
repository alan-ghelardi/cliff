(ns cliff.parser
  (:require [cliff.built-in :as built-in]
            [cliff.help :as help]
            [clojure.string :as string]))

(defn- interrupt [context]
  (assoc context ::interrupt? true))

(defn- interrupt? [context]
  (::interrupt? context))

(defn- error [reason message]
  (interrupt {:status  :error
              :reason  reason
              :message message}))

(defn- printable-token [{:keys [current-token raw-token token-type]}]
  (apply str (flatten [(string/replace (name token-type) #"-" " ")
                       " "
                       "'" (or current-token raw-token) "'"
                       (when (= :shorthand-flag token-type)
                         [" in " raw-token])])))

(defn- unknown-token [context]
  (error :unknown-token (str "Unknown " (printable-token context))))

(defn- unparseable-value [{:keys [current-value token-type] :as context} {:keys [name type]}]
  (error :unparseable-value (format "Unparseable value for %s. Expected '%s', but got '%s'"
                                    (if (= :argument token-type)
                                      (format "argument '%s'" (help/argument-name name))
                                      (printable-token context))
                                    (clojure.core/name type) current-value)))

(defn- apply-defaults [{:keys [arguments long-flags] :as context}]
  (reduce (fn [context [_ {:keys [name default]}]]
            (if (and default (nil? (get-in context [:result name])))
              (assoc-in context [:result name] default)
              context))
          context (merge arguments long-flags)))

(defn- check-missing-arguments [{:keys [arguments long-flags result] :as context}]
  (let [find-missing-values (fn [str-fn errors [_ {:keys [name required?]}]]
                              (if (and required? (not (get result name)))
                                (conj errors (str-fn name))
                                errors))
        errors              (into (reduce (partial find-missing-values help/long-flag) [] long-flags)
                                  (reduce (partial find-missing-values help/argument-name) [] arguments))]
    (if (seq errors)
      (error :missing-required-arguments (str "The following required arguments are missing: " (help/sentence errors {:sort? true})))
      context)))

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
    (seq parsing-tokens)           (assoc context :current-value (apply str parsing-tokens) :parsing-tokens [])
    :else                          (assoc context :current-value (first args) :args (next args))))

(defn- validate-enum [{:keys [current-value] :as context} {:keys [values] :as attributes}]
  (if (values current-value)
    context
    (error :invalid-argument
           (format "Invalid argument '%s' for %s. Valid values are %s." (name current-value)
                   (printable-token context)
                   (help/sentence values {:sort? true :quote? true})))))

(defn- parse-arg-value [context attributes]
  (try
    (update context :current-value #(built-in/parse-value attributes %))
    (catch Exception e
      (unparseable-value context attributes))))

(defmacro interrupting-> [context & forms]
  (let [c            (gensym)
        let-bindings (reduce (fn [bindings form]
                               (into bindings `[~c (if (#'interrupt? ~c)
                                                     ~c
                                                     (-> ~c ~form))]))
                             [c context] forms)]
    `(let ~let-bindings
       ~c)))

(defn- assoc-arg-value* [context {:keys [values] :as attributes}]
  (letfn [(assoc-parsed-value [{:keys [current-value] :as context} {:keys [name list?]}]
            (if list?
              (update-in context [:result name] (fnil #(conj % current-value) []))
              (assoc-in context [:result name] current-value)))]
    (interrupting-> context
                    (parse-arg-value attributes)
                    (cond-> values (validate-enum attributes))
                    (assoc-parsed-value attributes))))

(defn- assoc-arg-value [context attributes]
  (letfn [(continue? [{:keys [args token-type] :as context} {:keys [list?]}]
            (and (not (interrupt? context))
                 (seq args)
                 (= token-type :argument)
                 list?))]
    (loop [context (assoc-arg-value* context attributes)]
      (if-not (continue? context attributes)
        context
        (recur (assoc-arg-value* (next-value context attributes) attributes))))))

(defn- parse-flag [{:keys [current-token long-flags shorthand-flags] :as context}]
  (if-let [attributes (get long-flags current-token
                           (get shorthand-flags current-token))]
    (assoc-arg-value (next-value context attributes) attributes)
    (unknown-token context)))

(defn- parse-shorthand-flags [context]
  (loop [{:keys [parsing-tokens] :as context}
         (parse-flag context)]
    (if (or (interrupt? context)
            (not (seq parsing-tokens)))
      context
      (recur (parse-flag (next-token context))))))

(defn- unknown-argument [{:keys [current-token subcommands?] :as context}]
  (if-not subcommands?
    (unknown-token context)
    (interrupt (assoc context :status :ok
                      :command current-token))))

(defn- parse-argument [{:keys [current-arg-position current-token] :as context
                        :or   {current-arg-position 0}}]
  (if-let [attributes (get-in context [:arguments current-arg-position])]
    (update (assoc-arg-value (assoc context :current-value current-token) attributes)
            :current-arg-position (fnil inc 0))
    (unknown-argument context)))

(defn- parse-token [{:keys [token-type] :as context}]
  (case token-type
    :shorthand-flag (parse-shorthand-flags context)
    :long-flag      (parse-flag context)
    :argument       (parse-argument context)))

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

(defn- parse-args [context]
  (letfn [(continue? [{:keys [args] :as context}]
            (and (not (interrupt? context))
                 (seq args)))]
    (loop [context context]
      (if-not (continue? context)
        context
        (recur (parse-next-arg context))))))

(defn parser-context [{:keys [arguments flags]} args options]
  (letfn [(get-arguments []
            (->> arguments
                 (map-indexed (partial vector))
                 (reduce #(merge %1 %2) {})))
          (get-flags []
                     (->> flags
                          (map (fn [[name {:keys [shorthand] :as flag}]]
                                 [(help/argument-name name) shorthand (assoc flag :name name)]))
                          (reduce (fn [result [long-flag shorthand-flag flag-attrs]]
                                    (-> result
                                        (assoc-in [:long-flags long-flag] flag-attrs)
                                        (cond-> shorthand-flag                                         (assoc-in [:shorthand-flags shorthand-flag] flag-attrs))))
                                  {})))]
    (merge options
           (assoc (get-flags)
                  :arguments (get-arguments)
                  :args args))))

(defn parse
  ([program args] (parse program args {}))
  ([program args options]
   (let [output (interrupting-> program
                                (parser-context args options)
                                parse-args
                                apply-defaults
                                check-missing-arguments
                                (assoc :status :ok))]
     (select-keys output [:status :reason :message :args :command :result]))))
