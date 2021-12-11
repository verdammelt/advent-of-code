(in-package #:aoc)

(defun sum (nums)
  "Return the sum of NUMS"
  (reduce #'+ nums))

(defun product (nums)
  "Return the product of NUMS"
  (reduce #'* nums))
