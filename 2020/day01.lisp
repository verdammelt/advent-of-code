(defpackage #:aoc-2020-01
  (:use :cl))

(in-package #:aoc-2020-01)

(aoc:def-today-suite*)

(defparameter +data+
  (aoc:read-data (aoc:today-data-pathname) :line-parser #'parse-integer))

(defparameter +short-data+
  (aoc:read-data (aoc:today-data-pathname "example") :line-parser #'parse-integer))

(defun sums-to-p (target) (lambda (&rest ns) (= target (apply #'+ ns))))

(defun nums-sum-to (target) (lambda (xy) (apply (sums-to-p target) xy)))

(defun combinations-3 (list)
  (loop for (a1 . r1) on list
        nconc (loop for (a2 . r2) on r1
                 nconc (loop for a3 in r2 collect (list a1 a2 a3)))))

(defun combo-that-sums-to-2020 (data combo-maker)
  (let ((pair (find-if (nums-sum-to 2020) (funcall combo-maker data))))
    (list pair (apply #'* pair))))

(defun part1 (data)
  (second (combo-that-sums-to-2020 data #'aoc-2020/utils:combo-pairs))  )


(5am:def-test part1 (:suite :aoc-2020-01)
  (5am:is (= 514579 (part1 +short-data+)))
  (5am:is (= 252724 (part1 +data+))))

(defun part2 (data)
  (second (combo-that-sums-to-2020 data #'combinations-3))  )

(5am:def-test part2 (:suite :aoc-2020-01)
  (5am:is (= 241861950 (part2 +short-data+)))
  (5am:is (= 276912720 (part2 +data+))))
