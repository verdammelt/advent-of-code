(defpackage #:aoc-2020-06
  (:use :cl #:aoc-2020/utils))

(in-package #:aoc-2020-06)

(defparameter +input+ (aoc:read-data (aoc:data-pathname "day06" "txt")
                                     :pre-process #'split-on-empty-line))
(defparameter +example+ (aoc:read-data (aoc:data-pathname "day06-example" "txt")
                                       :pre-process #'split-on-empty-line))

(defun combine-group (group)
  (remove-duplicates (join-strings "" group)))

(defun count-all-answers (group)
  (length (combine-group group)))

(defun count-all-answers-in-groups (input)
  (apply #'+ (mapcar #'count-all-answers input)))

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
