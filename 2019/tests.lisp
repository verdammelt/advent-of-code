(defpackage #:aoc-2019/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2019/test)

(def-suite :aoc-2019)

(defun run-tests (&optional day)
  (aoc:run-tests 2019 day))
