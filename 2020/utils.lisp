(defpackage #:aoc-2020/utils
  (:use :cl)
  (:export :split-on-empty-line :join-strings
           :today-data
           :combo-pairs))

(in-package #:aoc-2020/utils)

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
