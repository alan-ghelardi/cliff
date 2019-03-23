(ns cliff.parser-test
  (:refer-clojure :exclude [sort])
  (:require [cliff.parser :as parser]
            [clojure.test :refer :all]
            [matcher-combinators.test]))

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

(def get-role-policy {:flags {:role-name   {:type      :string
                                            :required? true}
                              :policy-name {:type      :string
                                            :required? true}}})

(def ls {:arguments [{:name    :file-name
                      :type    :string
                      :default "."}]
         :flags     {:all?          {:shorthand "a"
                                     :type      :boolean}
                     :color         {:type   :keyword
                                     :values #{:always :auto :never}}
                     :sort-by-time? {:shorthand "t"
                                     :type      :boolean}
                     :width         {:shorthand "w"
                                     :type      :int
                                     :default   0}}})

(def sort {:arguments [{:name :file-name
                        :type :string}]
           :flags     {:key-def {:shorthand "k"
                                 :type      :string
                                 :list?     true}}})

(def sum {:arguments [{:name :numbers
                       :type :double
                       :list? true}]})

(deftest parse-test
  (testing "parses shorthand flags"
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true}}
                (parser/parse ls ["-a" "-t"])))

    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true}}
                (parser/parse ls ["-t" "-a"]))
        "the order doesn't affect the final result"))

  (testing "parses shorthand flags that take arguments"
    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "-w" "10"])))
    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "-w10"]))))

  (testing "parses long flags"
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true}}
                (parser/parse ls ["--all" "--sort-by-time"])))

    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true}}
                (parser/parse ls ["--sort-by-time" "--all"]))
        "the order doesn't affect the final result"))

  (testing "parses long flags that take arguments"
    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "--width" "10"])))
    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "--width10"]))))

  (testing "uses `=` to assign arguments to options"
    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "--width=10"])))

    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "--width=" "10"])))

    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "-w=10"]))))

  (testing "shorthand flags can be grouped together"
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true}}
                (parser/parse ls ["-at"]))))

  (testing "groups multiple shorthand flags together and assign an argument to
  the last one"
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true :width 10}}
                (parser/parse ls ["-atw" "10"])))
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true :width 10}}
                (parser/parse ls ["-atw10"])))
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true :width 10}}
                (parser/parse ls ["-atw=10"]))))

  (testing "repeats a flag that accept a list of arguments"
    (are [args key-def] (match? {:status :ok
                                 :result {:file-name "customers.txt"
                                          :key-def key-def}} (parser/parse sort args))
      ["-k" "2,2" "-k" "1,1" "customers.txt"] ["2,2" "1,1"]
      ["-k" "1" "customers.txt"]              ["1"]
      ["--key-def" "1" "customers.txt"]       ["1"]
      ["-k" "1,1" "customers.txt" "-k" "2,2"] ["1,1" "2,2"]))

  (testing "when the key `:values` is set, ensures that the flag in question
    always takes one of the valid values"
    (is (match? {:status :ok
                 :result {:file-name "." :color :auto}}
                (parser/parse ls ["--color" "auto"])))

    (is  (match? {:status  :error
                  :message "Invalid argument 'none' for long flag 'color'. Valid values are 'always', 'auto' and 'never'."}
                 (parser/parse ls ["--color" "none"]))))

  (testing "parses positional arguments"
    (is (match? {:status :ok
                 :result {:file-name "Documents"}}
                (parser/parse ls ["Documents"])))

    (is (match? {:status :ok
                 :result {:source "customers.txt" :target "../Documents"}}
                (parser/parse cp ["customers.txt" "../Documents"]))))

  (testing "parses arguments and flags"
    (is (match? {:status :ok
                 :result {:file-name     "Documents"
                          :all?          true
                          :sort-by-time? true
                          :width         15}}
                (parser/parse ls ["-at" "-w" "15" "Documents"]))
        "first the argument, then the flags")

    (is (match? {:status :ok
                 :result {:file-name     "Documents"
                          :all?          true
                          :sort-by-time? true
                          :width         15}}
                (parser/parse ls ["Documents" "-at" "-w" "15"]))
        "first the flags, then the argument")

    (is (match? {:status :ok
                 :result {:file-name     "Documents"
                          :all?          true
                          :sort-by-time? true
                          :width         15}}
                (parser/parse ls ["-at" "Documents" "-w" "15"]))
        "the argument between the flags"))

  (testing "parses arguments that should be processed as list of values"
    (are [args result] (match? {:status :ok
                                :result {:image "bash"
                                         :args result}}
                               (parser/parse docker-run args))
      ["--rm" "bash" "ls"]              ["ls"]
      ["--rm" "bash" "ls" "-la"]        ["ls" "-la"]
      ["--rm" "bash" "ls" "-la" "/bin"] ["ls" "-la" "/bin"]))

  (testing "parsing errors"
    (are [program args reason message] (match? {:status :error
                                                :reason reason
                                                :message message} (parser/parse program args))
      ls ["-lt"]                      :unknown-token     "Unknown shorthand flag 'l' in -lt"
      ls ["-at" "--sort-by-name" "."] :unknown-token     "Unknown long flag '--sort-by-name'"
      cp ["-r" "folder" "../" "foo"]  :unknown-token     "Unknown argument 'foo'"
      ls ["-w" "hello"]               :unparseable-value "Unparseable value for shorthand flag 'w' in -w. Expected 'int', but got 'hello'"
      sum ["0" "abc"] :unparseable-value "Unparseable value for argument 'numbers'. Expected 'double', but got 'abc'"))

  (testing "default values for arguments and flags"
    (are [args result] (match? {:status :ok
                                :result result}
                               (parser/parse ls args))
      []              {:file-name "." :width 0}
      ["-ta"]         {:file-name "." :width 0}
      ["~/Documents"] {:file-name "~/Documents" :width 0}
      ["-w" "5"]      {:file-name "." :width 5}))

  (testing "missing mandatory arguments"
    (are [program args message] (match? {:status :error
                                         :reason :missing-required-arguments
                                         :message message}
                                        (parser/parse program args))
      cp              []                         "The following required arguments are missing: source and target"
      cp              ["-r" "folder"]            "The following required arguments are missing: target"
      get-role-policy []                         "The following required arguments are missing: --role-name and --policy-name"
      get-role-policy ["--role-name" "foo-role"] "The following required arguments are missing: --policy-name")))
