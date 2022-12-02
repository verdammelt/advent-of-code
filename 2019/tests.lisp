(defpackage #:aoc-2019/test
  (:use :cl :5am)
  (:export :run-tests))

(in-package #:aoc-2019/test)

(def-suite :aoc-2019)

(defun run-tests (&optional day)
  (run! (alexandria:make-keyword (format nil "AOC-2019~@[-~2,'0D~]" day))))
