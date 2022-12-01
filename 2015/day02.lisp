(defpackage #:aoc-2015-02
  (:use :cl))

(in-package #:aoc-2015-02)

(aoc:def-today-suite*)

(defun parse-line (line)
  (mapcar #'parse-integer (aoc:split-string-on-char #\x line)))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-line))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun surface-area (l w h)
  (+ (* 2 l w)
     (* 2 w h)
     (* 2 h l)))

(defun volume (l w h)
  (* l w h))

(defun smallest-side-dimensions (dimensions)
  (subseq (sort dimensions #'<) 0 2))

(defun a-little-extra (dimensions)
  (apply #'* (smallest-side-dimensions dimensions)))

(defun square-feet-of-paper (dimensions)
  (+ (apply #'surface-area dimensions)
     (a-little-extra dimensions)))

(defun total-square-feet (inputs)
  (apply #'+ (mapcar #'square-feet-of-paper inputs)))

(defun part1 (input) (total-square-feet input))

(5am:def-test part1 (:suite :aoc-2015-02)
  (5am:is (= 58 (part1 '((2 3 4)))))
  (5am:is (= 43 (part1 '((1 1 10)))))
  (5am:is (=  #.(+ 58 43) (part1 '((2 3 4) (1 1 10)))))

  (5am:is (= 1588178 (part1 +input+))))

(defun smallest-perimeter (dimensions)
  (* 2 (apply #'+ (smallest-side-dimensions dimensions))))

(defun perfect-bow-size (dimension)
  (apply #'volume dimension))

(defun ribbon-length (dimension)
  (+ (smallest-perimeter dimension)
     (perfect-bow-size dimension)))

(defun total-ribbon-size (inputs)
  (apply #'+ (mapcar #'ribbon-length inputs)))

(defun part2 (input) (total-ribbon-size input))

(5am:def-test part2 (:suite :aoc-2015-02)
  (5am:is (= 34 (part2 '((2 3 4)))))
  (5am:is (= 14 (part2 '((1 1 10)))))
  (5am:is (= #.(+ 34 14) (part2 '((2 3 4) (1 1 10)))))
  (5am:is (= 3783758 (part2 +input+))))
