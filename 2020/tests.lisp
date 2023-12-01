(defpackage #:aoc-2020/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2020/test)

(def-suite :aoc-2020)

(defun run-tests (&optional day)
  (aoc:run-tests 2020 day))
