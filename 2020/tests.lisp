(defpackage #:aoc-2020/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2020/test)

(def-suite :aoc-2020)

(defun run-tests (&optional day)
  (run! (alexandria:make-keyword (format nil "AOC-2020~@[-~2,'0D~]" day))))
