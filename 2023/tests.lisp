(defpackage #:aoc-2023/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2023/test)

(def-suite :aoc-2023)

(defun run-tests (&optional day)
  (run! (alexandria:make-keyword (format nil "AOC-2023~@[-~2,'0D~]" day))))
