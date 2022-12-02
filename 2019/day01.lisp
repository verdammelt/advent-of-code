(defpackage #:aoc-2019-01
  (:use :cl))

(in-package #:aoc-2019-01)

(aoc:def-today-suite*)

(defun fuel-cost (mass)
  (- (floor mass 3) 2))

(5am:def-test fuel-cost (:suite :aoc-2019-01)
  (5am:is (= 2 (fuel-cost 12)))
  (5am:is (= 2 (fuel-cost 14)))
  (5am:is (= 654 (fuel-cost 1969)))
  (5am:is (= 33583 (fuel-cost 100756))))

(defun parse-line (line)
  (with-input-from-string (stream line) (read stream)))

(defun parse-file (file)
  (mapcar #'parse-line (aoc:read-data file)))

(defun part1 (input-file)
  (let* ((masses (parse-file input-file))
         (fuel-costs (mapcar #'fuel-cost masses)))
    (reduce #'+ fuel-costs)))

(5am:def-test part1 (:suite :aoc-2019-01)
  (5am:is (= 3454026 (part1 (aoc:today-data-pathname)))))

(defun total-fuel-cost (mass)
  (loop for cost = (fuel-cost mass) then (fuel-cost cost)
        while (plusp cost)
        sum cost))

(5am:def-test total-fuel-cost (:suite :aoc-2019-01)
  (5am:is (= 2 (total-fuel-cost 12)))
  (5am:is (= 966 (total-fuel-cost 1969)))
  (5am:is (= 50346 (total-fuel-cost 100756))))

(defun part2 (input-file)
  (let* ((masses (parse-file input-file))
         (fuel-costs (mapcar #'total-fuel-cost masses)))
    (reduce #'+ fuel-costs)))

(5am:def-test part2 (:suite :aoc-2019-01)
  (5am:is (= 5178170 (part2 (aoc:today-data-pathname)))))
