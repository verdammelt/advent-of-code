(defpackage #:aoc-2020-06
  (:use :cl #:aoc-2020/utils))

(in-package #:aoc-2020-06)

(aoc:def-today-suite*)

(defparameter +input+ (aoc:read-data (aoc:today-data-pathname)
                                     :pre-process #'split-on-empty-line))
(defparameter +example+ (aoc:read-data (aoc:today-data-pathname "example")
                                       :pre-process #'split-on-empty-line))

(defun combine-group (group)
  (remove-duplicates (join-strings "" group)))

(defun count-all-answers (group)
  (length (combine-group group)))

(defun count-all-answers-in-groups (input)
  (apply #'+ (mapcar #'count-all-answers input)))

(5am:test part1
  (5am:is (= 11 (count-all-answers-in-groups +example+)))
  (5am:is (= 6735 (count-all-answers-in-groups +input+))))

(defun count-common-answers (group)
  (length
   (reduce #'(lambda (result answer)
               (if (every #'(lambda (person) (find answer person)) group)
                   (push answer result)
                   result))
           (combine-group group)
           :initial-value (list))))

(defun count-common-answers-in-groups (input)
  (apply #'+ (mapcar #'count-common-answers input)))

(5am:test part2
  (5am:is (= 6 (count-common-answers-in-groups +example+)))
  (5am:is (= 3221 (count-common-answers-in-groups +input+))))
