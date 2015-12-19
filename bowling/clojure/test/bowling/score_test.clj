(ns bowling.score-test
  (:require [clojure.test :refer :all]
            [bowling.score :refer [score]]))

(deftest init-test
  (testing "score with invalid rolls"
    (is (= (score [1 1  1 1  1 1  1 -3  1 1  1 1  1 1  1 1  1 1  1 1]) :error))
    (is (= (score [1 1  1 1  1 1  -3 1  1 1  1 1  1 1  1 1  1 1  1 1]) :error))
    (is (= (score [1 1  1 1  1 1  12    1 1  1 1  1 1  1 1  1 1  1 1]) :error))
    (is (= (score [1 1  1 1  1 1  5 6   1 1  1 1  1 1  1 1  1 1  1 1]) :error)))
  (testing "score with no rolls"
    (is (= (score []) 0)))
  (testing "score with all strikes"
    (is (= 300 (score [10   10   10   10   10   10   10   10   10   10   10   10]))))
  (testing "score with extra rolls"
    (is (= 300 (score [10   10   10   10   10   10   10   10   10   10   10   10  10  10  10])))) ; extras
  (testing "score for various complete games"
    (is (= 0   (score [0 0  0 0  0 0  0 0  0 0  0 0  0 0  0 0  0 0  0 0])))
    (is (= 20  (score [1 1  1 1  1 1  1 1  1 1  1 1  1 1  1 1  1 1  1 1])))
    (is (= 150 (score [5 5  5 5  5 5  5 5  5 5  5 5  5 5  5 5  5 5  5 5  5])))
    (is (= 47  (score [1 1  1 1  1 1  1 1  1 1  1 1  1 1  1 1  1 1  10   10   9])))
    (is (= 173 (score [7 3  7 3  7 3  7 3  7 3  7 3  7 3  7 3  7 3  7 3  10])))
    (is (= 240 (score [10   10   10   0 0  10   10   10   10   10   10   10   10])))
    (is (= 245 (score [10   10   10   10   10   10   10   10   10   1 1])))))
