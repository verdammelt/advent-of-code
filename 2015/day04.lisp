(defpackage #:aoc-2015-04
  (:use :cl))

(in-package #:aoc-2015-04)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :post-process #'first))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun make-coin (secret idx)
  (format nil "~{~2,'0X~}"
          (coerce (md5:md5sum-string (format nil "~A~D" secret idx)) 'list)))

(defun leading-zeros (n)
  (lambda (coin)
    (string= (make-string  n  :initial-element #\0) (subseq coin 0 n))))

(defun first-advent-coin-idx (secret &optional (advent-coin-p (leading-zeros 5)))
  (loop for x from 0
     when (funcall advent-coin-p (make-coin secret x))
     do (return x)))

(defun part1 (input) (first-advent-coin-idx input))

(5am:def-test part1 (:suite :aoc-2015-04)
  (5am:is (= 609043 (part1 "abcdef")))
  (5am:is (= 1048970 (part1 "pqrstuv")))

  (5am:is (= 117946 (part1 +input+))))

(defun part2 (input) (first-advent-coin-idx input (leading-zeros 6)))

(5am:def-test part2 (:suite :aoc-2015-04)
  (5am:is (= 3938038 (part2 +input+))))
