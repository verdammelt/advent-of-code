(defpackage #:aoc-2020-18
  (:use :cl #:aoc))

(in-package #:aoc-2020-18)

(5am:def-suite :aoc-2020-18 :in :aoc-2020)

(defun parse (str)
  (read-from-string (format nil "(~A)" str)))

(defun evaluate (expr)
  (cond ((atom expr) expr)
        ((= (length expr) 1) (car expr))
        ((>= (length expr) 3)
         (destructuring-bind (arg1 op arg2 &rest more) expr
           (evaluate (push (funcall op (evaluate arg1) (evaluate arg2)) more))))
        (t (error "Invalid expression: ~S" expr))))

(defparameter +input+ (read-data (today-data-pathname) :line-parser #'parse))

(defun part1 (input)
  (reduce #'+ (mapcar #'evaluate input)))

(5am:def-test part1 (:suite :aoc-2020-18)
  (5am:is (= 71 (evaluate (parse "1 + 2 * 3 + 4 * 5 + 6"))))
  (5am:is (= 51 (evaluate (parse "1 + (2 * 3) + (4 * (5 + 6))"))))
  (5am:is (= 26 (evaluate (parse "2 * 3 + (4 * 5)"))))
  (5am:is (= 437 (evaluate (parse "5 + (8 * 3 + 9 + 3 * 4 * 3)"))))
  (5am:is (= 12240 (evaluate (parse "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))))
  (5am:is (= 13632 (evaluate (parse "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))

  (5am:is (= 30753705453324 (part1 +input+))))

(defun evaluate2 (expr)
  (cond ((atom expr) expr)
        ((null (cdr expr)) (evaluate2 (car expr)))
        ((position '* expr)
         (let ((sub-exprs (split-sequence:split-sequence '* expr)))
           (reduce #'* (mapcar #'evaluate2 sub-exprs))))
        ((position '+ expr)
         (let ((sub-exprs (split-sequence:split-sequence '+ expr)))
           (reduce #'+ (mapcar #'evaluate2 sub-exprs))))
        (t (error "Invalid expression ~S" expr))))

(defun part2 (input)
  (reduce #'+ (mapcar #'evaluate2 input)))

(5am:def-test part2 (:suite :aoc-2020-18)
  (5am:is (= 231 (evaluate2 (parse "1 + 2 * 3 + 4 * 5 + 6"))))
  (5am:is (= 51 (evaluate2 (parse "1 + (2 * 3) + (4 * (5 + 6))"))))
  (5am:is (= 46 (evaluate2 (parse "2 * 3 + (4 * 5)"))))
  (5am:is (= 1445 (evaluate2 (parse "5 + (8 * 3 + 9 + 3 * 4 * 3)"))))
  (5am:is (= 669060 (evaluate2 (parse "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))))
  (5am:is (= 23340 (evaluate2 (parse "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))))

  (5am:is (= 244817530095503 (part2 +input+))))
