(defpackage :file-utils
  (:use :common-lisp)
  (:export read-lines))

(in-package :file-utils)

(defun read-lines (file)
  (with-open-file (input file)
    (loop
       :for line := (read-line input nil nil)
       :while line :collect line)))
