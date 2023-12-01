(defpackage #:aoc-2023-01
  (:use :cl))

(in-package #:aoc-2023-01)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun simple-digits-only (str)
  (remove-if-not #'digit-char-p str))

(defun digits-only (str)
  (coerce
   (loop for i below (length str)
         for s = (subseq str i) then (subseq str i)
         when (digit-char-p (char s 0))
           collect (char s 0)
         when (alexandria:starts-with-subseq "one" s)
           collect #\1
         when (alexandria:starts-with-subseq "two" s)
           collect #\2
         when (alexandria:starts-with-subseq "three" s)
           collect #\3
         when (alexandria:starts-with-subseq "four" s)
           collect #\4
         when (alexandria:starts-with-subseq "five" s)
           collect #\5
         when (alexandria:starts-with-subseq "six" s)
           collect #\6
         when (alexandria:starts-with-subseq "seven" s)
           collect #\7
         when (alexandria:starts-with-subseq "eight" s)
           collect #\8
         when (alexandria:starts-with-subseq "nine" s)
           collect #\9)
   'string))

(defun only-first-and-last (str)
  (format nil "~C~C"(char str 0) (char str (1- (length str)))))

(defun calibrate (raw digits-only-fn)
  (parse-integer (only-first-and-last (funcall digits-only-fn raw))))

(defun part1 (input)
  (aoc:sum (mapcar #'(lambda (str) (calibrate str #'simple-digits-only)) input)))

(5am:def-test part1 (:suite :aoc-2023-01)
  (5am:is (= 12 (calibrate "1abc2" #'simple-digits-only)))
  (5am:is (= 38 (calibrate "pqr3stu8vwx" #'simple-digits-only)))
  (5am:is (= 15 (calibrate "a1b2c3d4e5f" #'simple-digits-only)))
  (5am:is (= 77 (calibrate "treb7uchet" #'simple-digits-only)))
  (5am:is (= 54388 (part1 +input+))))

(defun part2 (input)
  (aoc:sum (mapcar #'(lambda (str) (calibrate str #'digits-only)) input)))

(5am:def-test part2 (:suite :aoc-2023-01)
  (5am:is (= 29 (calibrate "two1nine" #'digits-only)))
  (5am:is (= 83 (calibrate "eightwothree" #'digits-only)))
  (5am:is (= 13 (calibrate "abcone2threexyz" #'digits-only)))
  (5am:is (= 24 (calibrate "xtwone3four" #'digits-only)))
  (5am:is (= 42 (calibrate "4nineeightseven2" #'digits-only)))
  (5am:is (= 14 (calibrate "zoneight234" #'digits-only)))
  (5am:is (= 76 (calibrate "7pqrstsixteen" #'digits-only)))
  (5am:is (= 82 (calibrate "eightwo" #'digits-only))) ;; corner case not in official example !!
  (5am:is (= 53515 (part2 +input+))))
