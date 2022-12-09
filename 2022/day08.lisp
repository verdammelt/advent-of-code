(defpackage #:aoc-2022-08
  (:use :cl))

(in-package #:aoc-2022-08)

(aoc:def-today-suite*)

(defun string->number-list (str)
  "Transform STR into a list of numbers. e.g.: \"123\" => '(1 2 3)"
  (mapcar #'digit-char-p (coerce str 'list)))

(defun read-data (file) (aoc:read-data file :line-parser #'string->number-list))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun is-visible-p (arr row col)
  (let ((tree (aref arr row col)))
    (or
     ;; everything earlier in the row
     (loop for r from 0 below row
           when (<= tree (aref arr r col))
             do (return nil)
           finally (return t))

     ;; everything later in the row
     (loop for r from (1+ row) below (array-dimension arr 0)
           when (<= tree (aref arr r col))
             do (return nil)
           finally (return t))

    ;; everything earlier in the column
     (loop for c from 0 below col
           when (<= tree (aref arr row c))
             do (return nil)
           finally (return t))

     ;; everything later in the column
     (loop for c from (1+ col) below (array-dimension arr 1)
           when (<= tree (aref arr row c))
             do (return nil)
           finally (return t)))))

(defun visible-from-outside (input)
  (let ((forest (aoc:lists->2d-array input))
        (row-max (1- (length input)))
        (col-max (1- (length (first input)))))
    (aoc:map-2d-array
     #'(lambda (arr row column)
         (cond ((or (zerop row) (zerop column)) t)
               ((or (= row-max row) (= col-max column)) t)
               (t (is-visible-p arr row column))))
     forest)))

(defun part1 (input)
  (let ((visible (visible-from-outside input)))
    (loop for idx below (array-total-size visible)
          count (row-major-aref visible idx))))

(5am:def-test part1 (:suite :aoc-2022-08)
  (5am:is (= 21 (part1 +example+)))
  (5am:is (= 1870 (part1 +input+))))

(defun scenic-score-for-tree (forest col row)
  (let ((tree (aref forest row col))
        (max-row (array-dimension forest 0))
        (max-col (array-dimension forest 1)))
    (let ((scores
            (list
             ;; items left?
             (loop for c from (1- col) downto 0
                   for n = 1 then (incf n)
                   when (<= tree (aref forest row c))
                     do (return n)
                   finally (return n))
             ;; items right?
             (loop for c from (1+ col) below max-col
                   for n = 1 then (incf n)
                   when (<= tree (aref forest row c))
                     do (return n)
                   finally (return n))
             ;; items above?
             (loop for r from (1- row) downto 0
                   for n = 1 then (incf n)
                   when (<= tree (aref forest r col))
                     do (return n)
                   finally (return n))
             ;; items below?
             (loop for r from (1+ row) below max-row
                   for n = 1 then (incf n)
                   when (<= tree (aref forest r col))
                     do (return n)
                   finally (return n))
             )))
      (aoc:product (substitute 0 nil scores)))))

(defun scenic-scores (input)
  (let ((forest (aoc:lists->2d-array input)))
    (aoc:map-2d-array #'scenic-score-for-tree forest)))

(defun part2 (input)
  (let ((scenic-scores (scenic-scores input)))
    (loop for idx below (array-total-size scenic-scores)
          maximizing (row-major-aref scenic-scores idx))))

(5am:def-test part2 (:suite :aoc-2022-08)
  (5am:is (= 8 (part2 +example+)))
  (5am:is (= 517440 (part2 +input+))))
