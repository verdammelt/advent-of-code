(defpackage #:aoc-2021-02
  (:use :cl))

(in-package #:aoc-2021-02)

(aoc:def-today-suite*)

(defun parse-command (line)
  (destructuring-bind (command-string number)
      (aoc:split-string-on-char #\Space line)
    (list (aoc:keywordize command-string)
          (parse-integer number))))

(defun read-data (file)
  (aoc:read-data file :line-parser #'parse-command))

(defparameter +example1+   (read-data (aoc:today-data-pathname "example")))

(defparameter +part1+ (read-data (aoc:today-data-pathname)))

(defun make-sub-data (horiz-position depth aim) (list horiz-position depth aim))
(defun horiz-position (data) (first data))
(defun depth (data) (second data))
(defun aim (data) (third data))
(defparameter +sub-init-data+ (make-sub-data 0 0 0))

(defun sub-1 (data command)
  (destructuring-bind (op value) command
    (ecase op
      (:forward (make-sub-data (+ (horiz-position data) value) (depth data) 0))
      (:down (make-sub-data (horiz-position data) (+ (depth data) value) 0))
      (:up (make-sub-data (horiz-position data) (- (depth data) value) 0)))))

(defun follow-course (sub course)
  (reduce sub course :initial-value +sub-init-data+))

(defun compute-value (data) (* (horiz-position data) (depth data)))

(defun part1 (course)
  (compute-value (follow-course #'sub-1 course)))

(5am:def-test part1 (:suite :aoc-2021-02)
  (5am:is (= 150 (part1 +example1+)))
  (5am:is (= 2322630 (part1 +part1+))))

(defun sub-2 (data command)
  (destructuring-bind (op value) command
      (ecase op
        (:forward (make-sub-data (+ (horiz-position data) value)
                                 (+ (depth data) (* (aim data) value))
                                 (aim data)))
        (:down (make-sub-data (horiz-position data) (depth data) (+ (aim data) value)))
        (:up  (make-sub-data (horiz-position data) (depth data) (- (aim data) value))))))

(defun part2 (course)
  (compute-value (follow-course #'sub-2 course)))

(5am:def-test part2 (:suite :aoc-2021-02)
  (5am:is (= 900 (part2 +example1+)))
  (5am:is (= 2105273490 (part2 +part1+))))
