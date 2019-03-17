(ns cliff.parser-test
  (:require [cliff.parser :as parser]
            [clojure.test :refer :all]
            [matcher-combinators.test]))

(def ls {:arguments [{:name    :file-name
                      :type    :string
                      :default "."}]
         :flags     {:all?          {:shorthand "a"
                                     :type      :boolean}
                     :sort-by-time? {:shorthand "t"
                                     :type      :boolean}
                     :width         {:shorthand "w"
                                     :type      :int
                                     :default   0}}})

(def cp {:arguments [{:name      :source
                      :type      :string
                      :required? true}
                     {:name      :target
                      :type      :string
                      :required? true}]
         :flags     {:recursive? {:type      :boolean
                                  :shorthand "r"}}})

(def get-role-policy {:flags {:role-name   {:type      :string
                                            :required? true}
                              :policy-name {:type      :string
                                            :required? true}}})

(deftest parse-test
  (testing "parses the supplied shorthand flags"
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true}}
                (parser/parse ls ["-a" "-t"]))))

  (testing "the order in which the flags are supplied doesn't affect the final result"
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true}}
                (parser/parse ls ["-t" "-a"]))))

  (testing "parses the supplied long flags"
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true}}
                (parser/parse ls ["--all" "--sort-by-time"]))))

  (testing "some shorthand flags can take values"
    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "-w" "10"])))
    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "-w10"]))))

  (testing "some long flags can take values too"
    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "--width" "10"])))
    (is (match? {:status :ok
                 :result {:all? true :width 10}}
                (parser/parse ls ["-a" "--width10"]))))

  (testing "assigns a value to its respective flag by a equals sign"
    (is (match? {:status :ok
                 :result {:all? true :width 0}}
                (parser/parse ls ["-a" "--width=0"])))
    (is (match? {:status :ok
                 :result {:all? true :width 0}}
                (parser/parse ls ["-a" "--width=" "0"]))))
  (testing "assigns a value to its respective flag by a equals sign"
    (is (match? {:status :ok
                 :result {:all? true :width 0}}
                (parser/parse ls ["-a" "--width=0"]))))

  (testing "shorthand flags can be grouped together"
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true}}
                (parser/parse ls ["-at"]))))

  (testing "combines multiple shorthand flags together and assigns a value to the last one"
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true :width 10}}
                (parser/parse ls ["-atw" "10"])))
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true :width 10}}
                (parser/parse ls ["-atw10"])))
    (is (match? {:status :ok
                 :result {:all? true :sort-by-time? true :width 10}}
                (parser/parse ls ["-atw=10"]))))

  (testing "parses a single positional argument"
    (is (match? {:status :ok
                 :result {:file-name "Documents"}}
                (parser/parse ls ["Documents"]))))

  (testing "parses a positional argument along with some flags"
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

  (testing "detects and returns parsing errors"
    (are [program args reason message] (match? {:status :error
                                                :reason reason
                                                :message message} (parser/parse program args))
      ls ["-lt"]                      :unknown-token     "Unknown shorthand flag 'l' in -lt"
      ls ["-at" "--sort-by-name" "."] :unknown-token     "Unknown long flag '--sort-by-name'"
      cp ["-r" "folder" "../" "foo"]  :unknown-token     "Unknown argument 'foo'"
      ls ["-w" "hello"]               :unparseable-value "Unparseable value 'hello' for shorthand flag 'w' in -w"))

  (testing "default values for arguments and flags"
    (are [args result] (match? {:status :ok
                                :result result}
                               (parser/parse ls args))
      []              {:file-name "." :width 0}
      ["-ta"]         {:file-name "." :width 0}
      ["~/Documents"] {:file-name "~/Documents" :width 0}
      ["-w" "5"]      {:file-name "." :width 5}))

  (testing "returns an error when required arguments are missing"
    (are [program args message] (match? {:status :error
                                         :reason :missing-required-arguments
                                         :message message}
                                        (parser/parse program args))
      cp              []                         "The following required arguments are missing: source and target"
      cp              ["-r" "folder"]            "The following required arguments are missing: target"
      get-role-policy []                         "The following required arguments are missing: --role-name and --policy-name"
      get-role-policy ["--role-name" "foo-role"] "The following required arguments are missing: --policy-name")))
