(defpackage #:aoc-2024-02
  (:use :cl))

(in-package #:aoc-2024-02)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file :line-parser #'aoc:string-of-numbers->list-of-numbers))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun all-increase-or-decrease-p (report)
  (let ((sorted-report (sort (copy-seq report) #'>)))
    (or (equal sorted-report report)
        (equal sorted-report (reverse report)))))

(defun diffs (report)
  (mapcar #'- report (cdr report)))

(defun safe-report-p (report)
  (let ((sorted-report (sort (copy-seq report) #'>)))
    (and (or (equal sorted-report report)
             (equal sorted-report (reverse report)))
         (every #'(lambda (n) (<= 1 n 3)) (diffs sorted-report))
         )))

(defun part1 (input)
  (count-if #'safe-report-p input))

(5am:def-test part1 (:suite :aoc-2024-02)
  (5am:is (= 2 (part1 +example+)))
  (5am:is (= 472 (part1 +input+))))

(defun collect-combinations-with-one-less (report)
  (let ((combos (list)))
    (alexandria:map-combinations
     #'(lambda (c) (push c combos))
     report
     :length (1- (length report))
     :copy t)
    combos))

(defun safe-report-with-dampening-p (report)
  (some #'safe-report-p
        (collect-combinations-with-one-less report)))

(defun part2 (input)
  (count-if #'safe-report-with-dampening-p input))

(5am:def-test part2 (:suite :aoc-2024-02)
  (5am:is (= 4 (part2 +example+)))
  (5am:is (= 520 (part2 +input+))))
