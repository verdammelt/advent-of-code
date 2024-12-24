(defpackage #:aoc-2024-23
  (:use :cl))

(in-package #:aoc-2024-23)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file :line-parser #'(lambda (s) (aoc:split-string-on-char #\- s))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun part1 (input) (declare (ignore input)) 0)

(5am:def-test part1 (:suite :aoc-2024-23)
  (5am:skip ":aoc-2024-23.1 not implemented")
  ;; (5am:is (= -1 (part1 +example+)))
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2024-23)
  (5am:skip ":aoc-2024-23.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
