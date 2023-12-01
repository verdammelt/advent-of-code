(defpackage #:aoc-2021/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2021/test)

(def-suite :aoc-2021)

(defun run-tests (&optional day)
  (aoc:run-tests 2021 day))
