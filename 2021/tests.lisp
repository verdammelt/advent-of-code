(defpackage #:aoc-2021/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2021/test)

(def-suite :aoc-2021)

(defun run-tests (&optional day)
  (run! (alexandria:make-keyword (format nil "AOC-2021~@[-~2,'0D~]" day))))
