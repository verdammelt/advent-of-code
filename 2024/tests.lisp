(defpackage #:aoc-2024/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2024/test)

(def-suite :aoc-2024)

(defun run-tests (&optional day)
  (aoc:run-tests 2024 day))
