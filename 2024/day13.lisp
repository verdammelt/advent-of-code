(defpackage #:aoc-2024-13
  (:use :cl))

(in-package #:aoc-2024-13)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'(lambda (s) (aoc:split-string-on-chars '(#\+ #\= #\,) s))
                 :post-process #'aoc:split-lines-on-empty-line))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun equation-parser (data)
  (let ((a1 (parse-integer (second (first data))))
        (a2 (parse-integer (fourth (first data))))
        (b1 (parse-integer (second (second data))))
        (b2 (parse-integer (fourth (second data))))
        (v1 (parse-integer (second (third data))))
        (v2 (parse-integer (fourth (third data)))))
    (list :a1 a1 :a2 a2 :b1 b1 :b2 b2 :v1 v1 :v2 v2)))

(defun equation-solver-brute (eq)
  (let ((a1 (getf eq :a1))
        (a2 (getf eq :a2))
        (b1 (getf eq :b1))
        (b2 (getf eq :b2))
        (v1 (getf eq :v1))
        (v2 (getf eq :v2)))
    (loop for a upto (min (floor v1 a1) (floor v2 a2))
          append (loop for b upto (min (floor v1 b1) (floor v2 b2))
                       when (and (= v1 (+ (* a a1) (* b b1)))
                                 (= v2 (+ (* a a2) (* b b2))))
                         collect (list a b)))))

(defun matrix-determinant (matrix)
  (assert (equal (array-dimensions matrix) '(2 2)))

  (- (* (aref matrix 0 0) (aref matrix 1 1))
     (* (aref matrix 0 1) (aref matrix 1 0))))

(defun matrix-scalar-multiplication (matrix scalar)
  (aoc:map-2d-array-values #'(lambda (v) (* v scalar)) matrix))

(defun compute-matrix-inverse (matrix)
  (let ((matrix-determinant (matrix-determinant matrix))
        (new (make-array '(2 2) :initial-contents (list (list (aref matrix 1 1)
                                                              (* -1 (aref matrix 0 1)))
                                                        (list (* -1 (aref matrix 1 0))
                                                              (aref matrix 0 0))))))
    (matrix-scalar-multiplication new (/ 1 matrix-determinant))))

(defun matrix-vector-multiplication (matrix vector)
  (make-array 2 :initial-contents
              (list (+ (* (aref matrix 0 0) (aref vector 0))
                       (* (aref matrix 0 1) (aref vector 1)))
                    (+ (* (aref matrix 1 0) (aref vector 0))
                       (* (aref matrix 1 1) (aref vector 1))))))

(defun equation-solver (eq)
  (let* ((A (make-array '(2 2) :initial-contents (list (list (getf eq :a1)
                                                             (getf eq :b1))
                                                       (list (getf eq :a2)
                                                             (getf eq :b2)))))
         (b (make-array '(2) :initial-contents (list (getf eq :v1) (getf eq :v2))))
         (solution (coerce (matrix-vector-multiplication (compute-matrix-inverse A) b) 'list)))
    (when (every #'integerp solution) (list solution))))

(defun solution-cost (sol)
  (let ((a-cost 3)
        (b-cost 1))
    (+ (* a-cost (first sol)) (* b-cost (second sol)))))

(defun part1 (input)
  (let* ((equations (mapcar #'equation-parser input))
         (solutions (aoc:flatten (mapcar #'equation-solver equations))))
    (aoc:sum (mapcar #'solution-cost solutions))))

(5am:def-test part1 (:suite :aoc-2024-13)
  (5am:is (= 480 (part1 +example+)))
  (5am:is (= 29023 (part1 +input+))))

(defun adjust-values (eq)
  (let ((adjustment 10000000000000))
    (incf (getf eq :v1) adjustment)
    (incf (getf eq :v2) adjustment)
    eq))

(defun part2 (input)
  (let* ((equations (mapcar #'adjust-values (mapcar #'equation-parser input)))
         (solutions (aoc:flatten (mapcar #'equation-solver equations))))
    (aoc:sum (mapcar #'solution-cost solutions))))

(5am:def-test part2 (:suite :aoc-2024-13)
  (5am:is (= 875318608908 (part2 +example+)))
  (5am:is (= 96787395375634 (part2 +input+))))
