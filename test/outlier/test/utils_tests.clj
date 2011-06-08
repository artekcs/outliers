(ns outlier.test.utils_tests
  (:use [outlier.core])
  (:use [clojure.test])
	(:use [outlier.utils])
	(:use [clojure.contrib.generic.math-functions]))

; test for median
(deftest median-check
	(is (= 49.5 (median (range 100)))))

; test for mean
(deftest median-check
	(is (= 49.5 (mean (range 100)))))

; test for stddev (entire and population)
(deftest stddev-check
	; entire set
	; actually, (= 28.866... (stddev (range 100)))
	(is (= 29 (round (stddev (range 100)))))
	; population-based
	; actually, (= 29.011... (stddev (range 100)))
	(is (= 29 (round (stddev (range 100) false)))))

; test for mode
(deftest mode-test
	(is (= '(2 4) (sort (mode [1 2 4 5 8 4 2 6]))))
	(is (= '(4) (mode [1 2 4 5 8 4 2 4])))
	(nil? (mode (range 100))))
