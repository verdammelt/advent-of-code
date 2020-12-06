(defpackage #:aoc-2020/utils
  (:use :cl)
  (:export :split-on-empty-line :join-strings))

(in-package #:aoc-2020/utils)

(defun empty-string-p (str) (zerop (length str)))

(defun split-on-empty-line (lines)
  (split-sequence:split-sequence-if #'empty-string-p lines))

;; stolen from STR:JOIN
(defun join-strings (separator strings)
  (let ((separator (if (string= separator "~") "~~" separator)))
    (format nil (concatenate 'string "~{~A~^" separator "~}") strings)))
