(defpackage #:aoc-2015-10
  (:use :cl))

(in-package #:aoc-2015-10)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file))

(defparameter +input+
  (first (read-data (aoc:today-data-pathname))))

;; stolen from rosetta code https://rosettacode.org/wiki/Look-and-say_sequence#Common_Lisp
(defun look-and-say (s)
  (let ((out (list (char s 0) 0)))
    (loop for x across s do
      (if (char= x (first out))
          (incf (second out))
          (setf out (list* x 1 out))))
    (format nil "~{~a~^~}" (nreverse out))))

(defun look-and-say-n-times (s n)
  (let ((result (look-and-say s)))
    (dotimes (i (1- n) result)
      (setf result (look-and-say result)))))

(defun part1 (input &optional (iterations 40))
  (length (look-and-say-n-times input iterations)))

(5am:def-test part1 (:suite :aoc-2015-10)
  (5am:is (string= "11" (look-and-say-n-times "1" 1)))
  (5am:is (string= "21" (look-and-say-n-times "1" 2)))
  (5am:is (string= "1211" (look-and-say-n-times "1" 3)))
  (5am:is (string= "111221" (look-and-say-n-times "1" 4)))
  (5am:is (string= "312211" (look-and-say-n-times "1" 5)))

  (5am:is (= 252594 (part1 +input+))))

(defun part2 (input)
  (part1 input 50))

(5am:def-test part2 (:suite :aoc-2015-10)
  (5am:is (= 3579328 (part2 +input+))))
