(ns cliff.help-test
  (:require [cliff.help :as help]
            [cliff.command-examples :refer [docker-run ls get-role-policy]]
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
    (is (= ["Options"
            ["-a," "--all" "" "" "do not ignore entries starting with ."]
            ["" "--color" "WHEN" "keyword" "colorize the output. WHEN can be 'always', 'auto' and 'never' (default auto)"]
            ["-t," "--sort-by-time" "" "" "sort by modification time, newest first"]
            ["-w," "--width" "COLS" "int" "set output width to COLS (default 0)"]]
           (help/flags-usage (:flags ls))))

    (is (= ["Options"
            ["" "--policy-name" "POLICY-NAME" "string" "name of the desired policy"]
            ["" "--role-name" "ROLE-NAME" "string" "name of the desired role"]]
           (help/flags-usage (:flags get-role-policy)))
        "sorts flags alphabetically and uses the flag name as the value name")

    (is (true?
         (nil? (help/flags-usage nil)))
        "returns an empty seq when there are no flags")))
