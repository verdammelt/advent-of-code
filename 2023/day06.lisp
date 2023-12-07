(defpackage #:aoc-2023-06
  (:use :cl))

(in-package #:aoc-2023-06)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data
   file
   :line-parser #'(lambda (s) (string-trim '(#\Space)
                                      (second (aoc:split-string-on-char #\: s))))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun number-of-ways-to-beat-record (time distance)
  (loop for press-time from 1 below time
        count (> (* (- time press-time) press-time) distance)))

;; Learned about the quadratic solution via reddit.
(defun quadratic-solution (a b c)
  (let ((d (sqrt (- (* b b) (* 4 a c)))))
    (if (zerop d)
        (values (/ (+ (- b) d) (* 2 a)))
        (values (/ (+ (- b) d) (* 2 a))
                (/ (- (- b) d) (* 2 a))))))

(defun number-of-wins-quadratic (time distance)
  (multiple-value-bind (x1 x2) (quadratic-solution -1 time (* -1 distance))
    (abs (- x1 x2))))

(defun parse-numbers-from-lines (lines)
  (mapcar #'aoc:string-of-numbers->list-of-numbers lines))

(defun part1 (input)
  (aoc:product (apply #'mapcar #'number-of-ways-to-beat-record
                      (parse-numbers-from-lines input))))

(5am:def-test part1 (:suite :aoc-2023-06)
  (5am:is (= 288 (part1 +example+)))
  (5am:is (= 840336 (part1 +input+))))

(defun parse-line-as-one-number (str)
  (parse-integer (remove #\Space str)))

(defun part2 (input)
  (apply #'number-of-ways-to-beat-record (mapcar #'parse-line-as-one-number input)))

(5am:def-test part2 (:suite :aoc-2023-06)
  (5am:is (= 71503 (part2 +example+)))
  (5am:is (= 41382569 (part2 +input+))))
