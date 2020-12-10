(defpackage #:aoc-2020-01
  (:use :cl))

(in-package #:aoc-2020-01)

(defparameter +data+
  (aoc:read-data (aoc:data-pathname "day01" "txt") :line-parser #'parse-integer))

(defparameter +short-data+
  (aoc:read-data (aoc:data-pathname "day01-example" "txt") :line-parser #'parse-integer))

(defun sums-to-p (target) (lambda (&rest ns) (= target (apply #'+ ns))))

(defun nums-sum-to (target) (lambda (xy) (apply (sums-to-p target) xy)))

(defun combinations-3 (list)
  (loop for (a1 . r1) on list
        nconc (loop for (a2 . r2) on r1
                 nconc (loop for a3 in r2 collect (list a1 a2 a3)))))

(defun combo-that-sums-to-2020 (data combo-maker)
  (let ((pair (find-if (nums-sum-to 2020) (funcall combo-maker data))))
    (list pair (apply #'* pair))))

(assert (= 514579 (second (combo-that-sums-to-2020 +short-data+ #'aoc-2020/utils:combo-pairs))))
(assert (= 252724 (second (combo-that-sums-to-2020 +data+ #'aoc-2020/utils:combo-pairs))))

(assert (= 241861950 (second (combo-that-sums-to-2020 +short-data+ #'combinations-3))))
(assert (= 276912720 (second (combo-that-sums-to-2020 +data+ #'combinations-3))))
