(ns cliff.command-examples)

(def aws {:flags {:region {:type :string}
                  :debug? {:type :boolean}}})

(def cp {:arguments [{:name      :source
                      :type      :string
                      :required? true}
                     {:name      :target
                      :type      :string
                      :required? true}]
         :flags     {:recursive? {:type      :boolean
                                  :shorthand "r"}}})

(def docker-run {:arguments [{:name :image
                              :type :string}
                             {:name  :args
                              :type  :string
                              :list? true}]
                 :flags     {:rm? {:type :boolean}}})

(def get-role-policy {:flags {:role-name   {:help      "name of the desired role"
                                            :type      :string
                                            :required? true}
                              :policy-name {:help      "name of the desired policy"
                                            :type      :string
                                            :required? true}}})

(def ls {:arguments [{:name    :file-name
                      :type    :string
                      :default "."}]
         :flags     {:all?          {:help      "do not ignore entries starting with ."
                                     :shorthand "a"
                                     :type      :boolean}
                     :color         {:help       "colorize the output"
                                     :type       :keyword
                                     :value-name "when"
                                     :values     #{:always :auto :never}
                                     :default    :auto}
                     :sort-by-time? {:help      "sort by modification time, newest first"
                                     :shorthand "t"
                                     :type      :boolean}
                     :width         {:help       "set output width to COLS"
                                     :shorthand  "w"
                                     :type       :int
                                     :value-name "cols"
                                     :default    0}}})

(def sort {:arguments [{:name :file-name
                        :type :string}]
           :flags     {:key-def {:shorthand "k"
                                 :type      :string
                                 :list?     true}}})

(def sum {:arguments [{:name  :numbers
                       :type  :double
                       :list? true}]})
