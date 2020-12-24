(defpackage #:aoc-2020/utils
  (:use :cl)
  (:export :split-on-empty-line :join-strings
           :today-data
           :combo-pairs
           :partial :rpartial
           :flatten))

(in-package #:aoc-2020/utils)

(5am:def-suite :aoc-2020)

(defun empty-string-p (str) (zerop (length str)))

(defun split-on-empty-line (lines)
  (split-sequence:split-sequence-if #'empty-string-p lines))

;; stolen from STR:JOIN
(defun join-strings (separator strings)
  (let ((separator (if (string= separator "~") "~~" separator)))
    (format nil (concatenate 'string "~{~A~^" separator "~}") strings)))

(defun today-data (&optional modifier)
  (aoc:data-pathname
   (format nil "day~2,'0D~@[-~A~]" (aoc:current-day) modifier)
   "txt"))

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
