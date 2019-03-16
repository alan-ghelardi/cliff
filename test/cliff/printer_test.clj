(ns cliff.printer-test
  (:require [cliff.printer :as printer]
            [clojure.test :refer :all]))

(deftest printable-argument-test
  (testing "returns a printable representation of the argument in question"
    (are [arg-type arg result] (= result (printer/printable-argument arg-type arg))
      :argument "source" "source"
      :long-flag "recursive" "--recursive"
      :shorthand-flag "r" "-r")))

(deftest sentence-test
  (testing "turns the seq of words into a human readable sentence"
    (are [words result] (= result (printer/sentence words))
      ["x"] "x"
      ["x" "y"] "x and y"
      ["x" "y" "z"] "x, y and z")))
