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
      ["x" "y" "z"] "x, y and z")))
