(defpackage #:aoc-2022/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2022/test)

(def-suite :aoc-2022)

(defun run-tests (&optional day)
  (run! (alexandria:make-keyword (format nil "AOC-2022~@[-~2,'0D~]" day))))
