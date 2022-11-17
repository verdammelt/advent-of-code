;; (load "../file-utils")

(defpackage :fuel-counter
  (:use :common-lisp))

(in-package :fuel-counter)

(defun fuel-cost (mass)
  (- (floor mass 3) 2))

(assert (= 2 (fuel-cost 12)))
(assert (= 2 (fuel-cost 14)))
(assert (= 654 (fuel-cost 1969)))
(assert (= 33583 (fuel-cost 100756)))

(defun parse-line (line)
  (with-input-from-string (stream line) (read stream)))

(defun parse-file (file)
  (mapcar #'parse-line (file-utils:read-lines (file-utils:file-in-day file 1))))

(defun part1 (input-file)
  (let* ((masses (parse-file input-file))
         (fuel-costs (mapcar #'fuel-cost masses)))
    (reduce #'+ fuel-costs)))

(assert (= 3454026 (part1 "./input.txt")))

(defun total-fuel-cost (mass)
  (loop for cost = (fuel-cost mass) then (fuel-cost cost)
        while (plusp cost)
        sum cost))

(assert (= 2 (total-fuel-cost 12)))
(assert (= 966 (total-fuel-cost 1969)))
(assert (= 50346 (total-fuel-cost 100756)))

(defun part2 (input-file)
  (let* ((masses (parse-file input-file))
         (fuel-costs (mapcar #'total-fuel-cost masses)))
    (reduce #'+ fuel-costs)))

(assert (= 5178170 (part2 "./input.txt")))
