(defpackage #:aoc-2015-01
  (:use :cl))

(in-package #:aoc-2015-01)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :post-process #'first))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun what-floor (input)
  (let ((char-hash (reduce #'(lambda (acc c) (incf (gethash c acc 0)) acc) input
                           :initial-value (make-hash-table))))
    (- (gethash #\( char-hash 0) (gethash #\) char-hash 0))))

(5am:def-test what-floor (:suite :aoc-2015-01)
  (let ((test-data '(("(())" . 0)
                     ("()()" . 0)
                     ("(((" . 3)
                     ("(()(()(" . 3)
                     ("))(((((" . 3)
                     ("())" . -1)
                     ("))(" . -1)
                     (")))" . -3)
                     (")())())" . -3))))
    (loop for (input . floor) in test-data
          do (5am:is (= (what-floor input) floor)))))

(defun part1 (input) (what-floor input))

(5am:def-test part1 (:suite :aoc-2015-01)
  (5am:is (= 280 (part1 +input+))))

(defun when-floor (input &optional (target-floor -1))
  (let ((current-floor 0))
    (1+ (position-if #'(lambda (delta) (= target-floor (incf current-floor delta)))
                     input
                     :key (lambda (c) (case c (#\( 1) (#\) -1)))))))

(5am:def-test when-floor (:suite :aoc-2015-01)
  (let ((test-data '((")" . 1)
                     ("()())" . 5))))
    (loop for (input . position) in test-data
          do (5am:is (= (when-floor input) position)))))

(defun part2 (input) (when-floor input))

(5am:def-test part2 (:suite :aoc-2015-01)
  (5am:is (= 1797 (part2 +input+))))
