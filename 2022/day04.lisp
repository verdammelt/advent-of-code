(defpackage #:aoc-2022-04
  (:use :cl))

(in-package #:aoc-2022-04)

(aoc:def-today-suite*)

(defun range (x y)
  "Produce a list of numbers from X to Y inclusive."
  (loop for i from x to y collect i))

(defun parse-pair (str)
  (let ((parts (mapcar #'parse-integer (aoc:split-string-on-chars '(#\- #\,) str))))
    (list (apply #'range (subseq parts 0 2))
          (apply #'range (subseq parts 2)))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-pair))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun full-overlap-p (pair)
  (let* ((r1 (first pair))
         (r2 (second pair))
         (common (intersection r1 r2)))
    (or (= (length common) (length r1))
        (= (length common) (length r2)))))

(defun part1 (input)
  (count t (mapcar #'full-overlap-p input)))

(5am:def-test part1 (:suite :aoc-2022-04)
  (5am:is (= 2 (part1 +example+)))
  (5am:is (= 580 (part1 +input+))))

(defun any-overlap-p (pair)
  (not (zerop (length (apply #'intersection pair)))))

(defun part2 (input)
  (count t (mapcar #'any-overlap-p input)))

(5am:def-test part2 (:suite :aoc-2022-04)
  (5am:is (= 4 (part2 +example+)))
  (5am:is (= 895 (part2 +input+))))
