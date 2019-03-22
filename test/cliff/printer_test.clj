(ns cliff.printer-test
  (:require [cliff.printer :as printer]
            [clojure.test :refer :all]))

(deftest keyword->arg-str-test
  (testing "turns the keyword into a string representing the argument in question"
    (are [key result] (= result (printer/keyword->arg-str key))
      :source     "source"
      :recursive? "recursive")))

(deftest argument-test
  (testing "returns a string representing the argument in question"
    (is (= "source" (printer/argument :source)))))

(deftest long-flag-test
  (testing "turns the keyword into a printable long flag"
    (are [flag-key result] (= result (printer/long-flag flag-key))
      :all        "--all"
      :recursive? "--recursive")))

(deftest sentence-test
  (testing "turns the seq of words into a human readable sentence"
    (are [words result] (= result (printer/sentence words))
      ["x"]         "x"
      ["x" "y"]     "x and y"
      ["x" "y" "z"] "x, y and z"
      ["z" "y" "x"] "z, y and x"))

  (testing "takes any Clojure object"
    (are [words sentence] (is (= sentence (printer/sentence words)))
      [:a :b] "a and b"
      [0 1 2] "0, 1 and 2"))

  (testing "returns a sentence with words sorted alphabetically"
    (is (= "x, y and z"
           (printer/sentence ["z" "y" "x"] {:sort? true}))))

  (testing "encloses each word in single quotes"
    (is (= "'x' and 'y'"
           (printer/sentence ["x" "y"] {:quote? true})))))
