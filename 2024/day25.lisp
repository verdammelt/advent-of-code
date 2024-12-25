(defpackage #:aoc-2024-25
  (:use :cl))

(in-package #:aoc-2024-25)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :post-process #'aoc:split-lines-on-empty-line))

(defun lockp (thing)
  (every #'(lambda (c) (char= c #\#)) (first thing)))

(defun partition (pred sequence)
  (reduce #'(lambda (acc item)
              (destructuring-bind (true false) acc
                (if (funcall pred item)
                    (push item true)
                    (push item false))

                (list true false)))
          sequence
          :initial-value (list (list) (list))))

(defparameter +example+
  (partition #'lockp (read-data (aoc:today-data-pathname "example"))))

(defparameter +input+
  (partition #'lockp (read-data (aoc:today-data-pathname))))

(defun lock-or-key-counts (lock-or-key-rows)
  (let ((counts (make-array 5)))
    (loop for row in lock-or-key-rows
          do (loop for col below 5
                   do (when (char= #\# (char row col))
                        (incf (elt counts col)))))
    counts))

(defun lock-counts (lock)
  ;; skip the first row
  (lock-or-key-counts (cdr lock)))

(defun key-counts (key)
  ;; skip the last row
  (lock-or-key-counts (butlast key)))

(defun possible-fit (lock key)
  (do ((overlap nil)
       (col 0 (incf col)))
      ((or overlap (= col 5)) (not overlap))

    (when (> (+ (elt lock col) (elt key col)) 5) (setf overlap t))))

(defun count-possible-fits (locks keys)
  (loop for lock in locks
        sum (count-if #'(lambda (key) (possible-fit (lock-counts lock) (key-counts key))) keys)))

(defun part1 (input)
  (count-possible-fits (first input) (second input)))

(5am:def-test part1 (:suite :aoc-2024-25)
  (5am:is (= 3 (part1 +example+)))
  (5am:is (= 3690 (part1 +input+))))
