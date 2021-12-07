(defpackage #:aoc-2020/utils
  (:use :cl)
  (:export :split-on-empty-line :join-strings
           :combo-pairs
           :partial :rpartial
           :flatten))

(in-package #:aoc-2020/utils)

(defun empty-string-p (str) (zerop (length str)))

(defun split-on-empty-line (lines)
  (aoc:split-lines-on-empty-line lines))

;; stolen from STR:JOIN
(defun join-strings (separator strings)
  (let ((separator (if (string= separator "~") "~~" separator)))
    (format nil (concatenate 'string "~{~A~^" separator "~}") strings)))

(defun combo-pairs (list)
  "Returns every pair of items from LIST"
  (loop for (a1 . r1) on list
        nconc (loop for a2 in r1 collect (list a1 a2))))

(defun partial (fn &rest args)
  "Partially applies ARGS to FN. Later args are appended to ARGS."
  (lambda (&rest other-args)
    (apply fn (append args other-args))))

(defun rpartial (fn &rest args)
  "Same as PARTIAL but args are in reverse order and appended to later ARGS"
  (lambda (&rest other-args)
    (apply fn (append other-args (reverse args)))))

(defun flatten (list-of-lists)
  (apply #'concatenate 'list list-of-lists))
