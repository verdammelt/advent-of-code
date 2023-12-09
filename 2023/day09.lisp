(defpackage #:aoc-2023-09
  (:use :cl))

(in-package #:aoc-2023-09)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file :line-parser #'aoc:string-of-numbers->list-of-numbers))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun compute-diffs (series)
  (loop for (a b) on series
        when b collect (- b a)))

(defun compute-diffs-until-zeros (series)
  (loop for s = series then (compute-diffs s)
        until (every #'zerop s)
        collect s))

(defun next-in-series (series delta)
  (+ (first (last series)) delta))

(defun extrapolate-value (series extrapolator)
  "SERIES is a list of numbers. Computes the next number in the series by first
finding the DIFF-SERIES (see COMPUTE-DIFFS-UNTIL-ZEROS and then uses this data
to find the next in series."
  (let ((diff-series (reverse (compute-diffs-until-zeros series))))
    (loop for next-in-series = 0
            then (funcall extrapolator series next-in-series)
          for series in diff-series
          finally (return next-in-series))))

(defun part1 (input)
  (flet ((extrapolate (series) (extrapolate-value series #'next-in-series)))
    (aoc:sum (mapcar #'extrapolate input))))

(5am:def-test part1 (:suite :aoc-2023-09)
  (5am:is (= 114 (part1 +example+)))
  (5am:is (= 1877825184 (part1 +input+))))

(defun previous-in-series (series delta)
  (- (first series) delta))

(defun part2 (input)
  (flet ((extrapolate (series) (extrapolate-value series #'previous-in-series)))
    (aoc:sum (mapcar #'extrapolate input))))

(5am:def-test part2 (:suite :aoc-2023-09)
  (5am:skip ":aoc-2023-09.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
