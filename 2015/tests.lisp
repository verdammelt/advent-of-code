(defpackage #:aoc-2015/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2015/test)

(def-suite :aoc-2015)

(defun run-tests (&optional day)
  (run! (alexandria:make-keyword (format nil "AOC-2015~@[-~2,'0D~]" day))))
