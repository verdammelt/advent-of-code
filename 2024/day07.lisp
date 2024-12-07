(defpackage #:aoc-2024-07
  (:use :cl))

(in-package #:aoc-2024-07)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data
   file
   :line-parser #'(lambda (str) (aoc:string-of-numbers->list-of-numbers
                            str :delimiters '(#\: #\ )))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defvar *operators* nil)

(defun %find-solutions (target current-value operands)
  (cond ((and (= current-value target) (null operands)) t)
        ((> current-value target) nil)
        ((null operands) nil)
        (t (some #'(lambda (op) (%find-solutions target
                                            (funcall op current-value (car operands))
                                            (cdr operands)))
                 *operators*)

)))

(defun has-solution-p (equation)
  (let ((target (car equation))
        (operands (cdr equation)))
    (%find-solutions target (car operands) (cdr operands))))

(defun part1 (input)
  (let ((*operators* (list #'* #'+)))
    (aoc:sum (mapcar #'first (remove-if-not #'has-solution-p input)))))

(5am:def-test part1 (:suite :aoc-2024-07)
  (5am:is (= 3749 (part1 +example+)))
  (5am:is (= 3245122495150 (part1 +input+))))

(defun || (n1 n2)
  (parse-integer (format nil "~D~D" n1 n2)))

(defun part2 (input)
  (let ((*operators* (list #'* #'+ #'||)))
    (aoc:sum (mapcar #'first (remove-if-not #'has-solution-p input)))))

(5am:def-test part2 (:suite :aoc-2024-07)
  (5am:is (= 11387 (part2 +example+)))
  (5am:is (= 105517128211543 (part2 +input+))))
