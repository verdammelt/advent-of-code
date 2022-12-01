(defpackage #:aoc-2022-01
  (:use :cl))

(in-package #:aoc-2022-01)

(aoc:def-today-suite*)

(defun partial (fn &rest args)
  "Partially applies ARGS to FN. Later args are appended to ARGS."
  (lambda (&rest other-args)
    (apply fn (append args other-args))))

(defun read-data (file) (aoc:read-data file
                                       :pre-process #'aoc:split-lines-on-empty-line
                                       :line-parser (partial #'mapcar #'parse-integer)))

(defun ordered-list-of-calories (list-of-snacks)
  (sort (mapcar #'aoc:sum list-of-snacks) #'>))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun part1 (input)
  (first (ordered-list-of-calories input)))

(5am:def-test part1 (:suite :aoc-2022-01)
  (5am:is (= 71780 (part1 +input+))))

(defun part2 (input)
  (aoc:sum (subseq (ordered-list-of-calories input) 0 3)))

(5am:def-test part2 (:suite :aoc-2022-01)
  (5am:is (= 212489 (part2 +input+))))
