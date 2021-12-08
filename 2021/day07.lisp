(defpackage #:aoc-2021-07
  (:use :cl))

(in-package #:aoc-2021-07)

(aoc:def-today-suite*)

(defun parse-crab-data (data)
  (mapcar #'parse-integer (aoc:split-string-on-char #\, data)))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-crab-data
                                       :post-process #'first))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +part1+
  (read-data (aoc:today-data-pathname)))

(defun average (nums)
  (/ (reduce #'+ nums) (length nums)))

(defun median (nums)
  (let ((sorted (sort (copy-seq nums) #'<))
        (length (length nums)))
    (if (oddp length)
        (nth (/ (1+ length) 2) sorted)
        (nth (/ length 2) sorted))))

(defun sum (nums) (reduce #'+ nums))

(defun sum-1-to-n (n) (/ (* n (1+ n)) 2))

(defun diff-from-target (target) (lambda (n) (abs (- n target))))

(defun fuel-cost (compute-target compute-fuel-cost-per-distance)
  (lambda (crabs)
    (flet ((from-target (target)
             (sum (mapcar compute-fuel-cost-per-distance
                          (mapcar (diff-from-target target) crabs)))))
      (let ((target (funcall compute-target crabs)))
        (if (integerp target)
            (values (list target (from-target target)))
            (values (list (floor target) (from-target (floor target)))
                    (list (ceiling target) (from-target (ceiling target)))))))))

(defun part1 (crabs)
  (second (funcall (fuel-cost #'median #'identity) crabs)))

(5am:def-test part1 (:suite :aoc-2021-07)
  (5am:is (= 37 (part1 +example+)))
  (5am:is (= 356922 (part1 +part1+))))

(defun part2 (crabs)
  (let* ((costs (multiple-value-list
                 (funcall (fuel-cost #'average #'sum-1-to-n) crabs)))
         (best (first (sort costs #'< :key #'second))))
    (second best)))

(5am:def-test part2 (:suite :aoc-2021-07)
  (5am:is (= 168 (part2 +example+)))
  (5am:is (= 100347031 (part2 +part1+))))
