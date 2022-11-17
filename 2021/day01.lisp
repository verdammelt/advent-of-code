(defpackage #:aoc-2021-01
  (:use :cl))

(in-package #:aoc-2021-01)

(aoc:def-today-suite*)

(defparameter +example1+
  (aoc:read-data (aoc:today-data-pathname "example") :line-parser #'parse-integer))

(defparameter +part1+
  (aoc:read-data (aoc:today-data-pathname) :line-parser #'parse-integer))

(defun split-into-chunks (size data)
  (remove nil
          (maplist
           #'(lambda (l) (when (>= (length l) size) (subseq l 0 size)))
           data)))

(defun sum-chunks (chunks)
  (mapcar #'(lambda (chunk) (apply #'+ chunk)) chunks))

(defun count-increases (data)
  (first
   (reduce #'(lambda (count-and-prev depth)
               (destructuring-bind (count prev-depth) count-and-prev
                 (list (+ count (if (> depth prev-depth) 1 0)) depth)))
           (rest data)
           :initial-value (list 0 (first data)))))

(defun count-increases-in-window-of-size (size data)
  (count-increases (sum-chunks (split-into-chunks size data))))

(defun part1 (data) (count-increases-in-window-of-size 1 data))

(5am:def-test part1 (:suite :aoc-2021-01)
  (5am:is (= 7 (part1 +example1+)))
  (5am:is (= 1553 (part1 +part1+))))

(defun part2 (data)
  (count-increases-in-window-of-size 3 data))

(5am:def-test part2 (:suite :aoc-2021-01)
  (5am:is (= 5 (part2 +example1+)))
  (5am:is (= 1597 (part2 +part1+))))
