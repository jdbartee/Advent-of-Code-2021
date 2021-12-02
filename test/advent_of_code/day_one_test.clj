(ns advent-of-code.day-one-test
  (:require [clojure.test :refer :all]
            [advent-of-code.day-one :refer :all]))

(deftest testing-did-increase?
  (testing "Verify did-increase? works as designed"
    (is (= 1 (did-increase? [1 2])))
    (is (= 0 (did-increase? [2 1])))
    (is (= 0 (did-increase? [2 2])))))

(deftest testing-sum3
  (testing "Verify sum3 works."
    (is (= 6 (sum3 [1 2 3])))
    (is (= 0 (sum3 [0 0 0])))))

(deftest testing-count-increases
  (testing "Count Increases actually counts the increases."
    (is (= 2 (count-increases [1 2 1 2])))
    (is (= 0 (count-increases [4 3 2 1])))
    (is (= 3 (count-increases [1 2 3 4])))))