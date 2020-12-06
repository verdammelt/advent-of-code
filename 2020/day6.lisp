(defpackage #:aoc-2020-06
  (:use :cl :aoc-2020/utils))

(in-package #:aoc-2020-06)

(defparameter +input+ (uiop:read-file-lines (aoc-2020/utils:data-pathname "day6")))
(defparameter +example+ (uiop:read-file-lines (aoc-2020/utils:data-pathname "day6-example")))

(defun empty-string-p (str) (zerop (length str)))

(defun split-groups (input)
  (split-on-empty-line input))

(defun combine-group (group)
  (remove-duplicates (join-strings "" group)))

(defun count-all-answers (group)
  (length (combine-group group)))

(defun count-all-answers-in-groups (input)
  (apply #'+ (mapcar #'count-all-answers (split-groups input))))

(defun count-common-answers (group)
  (length
   (reduce #'(lambda (result answer)
               (if (every #'(lambda (person) (find answer person)) group)
                   (push answer result)
                   result))
           (combine-group group)
           :initial-value (list))))

(defun count-common-answers-in-groups (input)
  (apply #'+ (mapcar #'count-common-answers (split-groups input))))
