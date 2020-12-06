(defpackage #:aoc-2020-01
  (:use :cl))

(in-package #:aoc-2020-01)

(defparameter +data+
  (aoc:read-data (aoc:data-pathname "day01" "txt") :line-parser #'parse-integer))

(defparameter +short-data+
  (aoc:read-data (aoc:data-pathname "day01-example" "txt") :line-parser #'parse-integer))

(defun sums-to-p (target) (lambda (&rest ns) (= target (apply #'+ ns))))

(defun nums-sum-to (target) (lambda (xy) (apply (sums-to-p target) xy)))

(defun combinations-2 (list)
  (loop for (a1 . r1) on list
        nconc (loop for a2 in r1 collect (list a1 a2))))

(defun combinations-3 (list)
  (loop for (a1 . r1) on list
        nconc (loop for (a2 . r2) on r1
                 nconc (loop for a3 in r2 collect (list a1 a2 a3)))))

(defun combo-that-sums-to-2020 (data combo-maker)
  (let ((pair (find-if (nums-sum-to 2020) (funcall combo-maker data))))
    (list pair (apply #'* pair))))