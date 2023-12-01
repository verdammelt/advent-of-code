(defpackage #:aoc-2022/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2022/test)

(def-suite :aoc-2022)

(defun run-tests (&optional day)
  (aoc:run-tests 2022 day))
