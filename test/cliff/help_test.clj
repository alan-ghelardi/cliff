(ns cliff.help-test
  (:require [cliff.command-examples :refer [cp get-role-policy ls docker sum]]
            [cliff.help :as help]
            [clojure.test :refer :all]))

(deftest argument-name-test
  (testing "turns the keyword into a string representing the argument in question"
    (are [key result] (= result (help/argument-name key))
      :source     "source"
      :recursive? "recursive")))

(deftest long-flag-test
  (testing "turns the keyword into a printable long flag"
    (are [flag-key result] (= result (help/long-flag flag-key))
      :all        "--all"
      :recursive? "--recursive")))

(deftest printable-str-test
  (testing "returns a printable string representing the Clojure object in question"
    (are [x result] (= result (help/printable-str x))
      :when "when"
      15    "15"
      "dir" "dir")))

(deftest sentence-test
  (testing "turns the seq of words into a human readable sentence"
    (are [words result] (= result (help/sentence words))
      ["x"]         "x"
      ["x" "y"]     "x and y"
      ["x" "y" "z"] "x, y and z"
      ["z" "y" "x"] "z, y and x"))

  (testing "takes any Clojure object"
    (are [words sentence] (is (= sentence (help/sentence words)))
      [:a :b] "a and b"
      [0 1 2] "0, 1 and 2"))

  (testing "returns a sentence with words sorted alphabetically"
    (is (= "x, y and z"
           (help/sentence ["z" "y" "x"] {:sort? true}))))

  (testing "encloses each word in single quotes"
    (is (= "'x' and 'y'"
           (help/sentence ["x" "y"] {:quote? true})))))

(deftest flags-usage-test
  (testing "returns usage information for a set of flags"
    (is (= ["Options:"
            ["-a," "--all" "" "" "do not ignore entries starting with ."]
            ["" "--color" "WHEN" "keyword" "colorize the output. WHEN can be 'always', 'auto' and 'never' (default auto)"]
            ["-t," "--sort-by-time" "" "" "sort by modification time, newest first"]
            ["-w," "--width" "COLS" "int" "set output width to COLS (default 0)"]]
           (help/flags-usage (:flags ls))))

    (is (= ["Options:"
            ["" "--policy-name" "POLICY-NAME" "string" "name of the desired policy"]
            ["" "--role-name" "ROLE-NAME" "string" "name of the desired role"]]
           (help/flags-usage (:flags get-role-policy)))
        "sorts flags alphabetically and uses the flag name as the value name")

    (is (true?
         (nil? (help/flags-usage nil)))
        "returns an empty seq when there are no flags")))

(deftest synopsis-test
  (testing "returns the synopsis for the command in question"
    (are [path command result] (= result (help/synopsis (assoc command :path path)))
      ["aws" "iam" "get-role-policy"] get-role-policy "aws iam get-role-policy <--policy-name=POLICY-NAME> <--role-name=ROLE-NAME>"
      ["cp"]                          cp              "cp [OPTIONS] <SOURCE> <TARGET>"
      ["docker"]                      docker          "docker <COMMAND>"
      ["ls"]                                ls              "ls [OPTIONS] [FILE-NAME]"
      ["sum"]                               sum             "sum [& NUMBERS]")))

(deftest available-commands-test
  (testing "returns information about the available commands"
    (is (= ["Commands:"
            ["ps" "List containers"]
            ["run" "Run a command in a new container"]]
           (help/available-commands (:commands docker)))))

  (testing "returns nil when there are no commands in the app"
    (is (nil? (help/available-commands (:commands ls))))))
