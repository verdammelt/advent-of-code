(defpackage #:aoc-2024-01
  (:use :cl))

(in-package #:aoc-2024-01)

(aoc:def-today-suite*)

(defun separate-pairs (pairs)
  (loop for (car cdr) in pairs
        collect car into cars
        collect cdr into cdrs
        finally (return (list cars cdrs))))

(defun parse-integer-pairs (pairs)
  (loop for (car cdr) in pairs
        collect (list (parse-integer car) (parse-integer cdr))))

(defun read-data (file)
  (aoc:read-data
   file
   :line-parser #'(lambda (l) (aoc:split-string-on-char #\Space l))
   :post-process #'(lambda (data) (separate-pairs (parse-integer-pairs data)))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun part1 (input)
  (aoc:sum
   (mapcar #'(lambda (x y) (abs (- x y)))
           (sort (copy-seq (first input)) #'<)
           (sort (copy-seq (second input)) #'<))))

(5am:def-test part1 (:suite :aoc-2024-01)
  (5am:is (= 11 (part1 +example+)))
  (5am:is (= 1879048 (part1 +input+))))

(defun frequencies (seq)
  (let ((counts (make-hash-table)))
    (loop for x in seq
          do (incf (gethash x counts 0)))
    counts))

(defun part2 (input)
  (let* ((left (first input))
         (right (second input))
         (counts (frequencies right)))
    (aoc:sum
     (mapcar #'(lambda (l) (* l (gethash l counts 0))) left))))

(5am:def-test part2 (:suite :aoc-2024-01)
  (5am:is (= 31 (part2 +example+)))
  (5am:is (= 21024792 (part2 +input+))))
