(defpackage #:aoc-2019-04
  (:use :cl))

(in-package #:aoc-2019-04)

(aoc:def-today-suite*)

(defparameter *puzzle-input* '(156218 . 652527))

(defun pairs-of (seq) (map 'list #'cons seq (subseq seq 1)))

(defun tuples-of (seq n)
  (loop for i from 0 upto (- (length seq) n)
        collect (subseq seq i (+ i n))))

(defun two-but-not-three-equal-p (guess)
  (let* ((pairs (tuples-of (coerce guess 'list) 2))
         (equal-pairs (remove-if-not #'(lambda (pair) (apply #'char= pair)) pairs)))
    (remove-if #'(lambda (pair) (> (count pair equal-pairs :test #'equal) 1)) equal-pairs)))

(defun has-equal-pair-p (guess)
  (let ((pairs (tuples-of (coerce guess 'list) 2)))
    (loop for pair in pairs
          when (apply #'char= pair) collect pair)))

(defun never-decreasing-p (guess)
  (let ((pairs (pairs-of guess)))
    (notany #'(lambda (pair) (char> (car pair) (cdr pair))) pairs)))

(defun check-guess-1 (guess)
  (and (= (length guess) 6)
       (has-equal-pair-p guess)
       (never-decreasing-p guess)))

(defun check-guess-2 (guess)
  (and (= (length guess) 6)
       (two-but-not-three-equal-p guess)
       (never-decreasing-p guess)))

(defun format-guess (guess)
  (format nil "~6,'0d" guess))

(defun guess-password (input-range check-guess)
  (loop with (low . high) = input-range
        for guess from low to high
        when (funcall check-guess (format-guess guess))
          collect guess))

(5am:def-test part1 (:suite :aoc-2019-04)
  (5am:is (= 1694 (length (guess-password *puzzle-input* #'check-guess-1)))))
(5am:def-test part2 (:suite :aoc-2019-04)
  (5am:is (= 1148 (length (guess-password *puzzle-input* #'check-guess-2)))))
