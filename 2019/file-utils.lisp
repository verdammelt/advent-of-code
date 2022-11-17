(defpackage :file-utils
  (:use :common-lisp)
  (:export read-lines file-in-day))

(in-package :file-utils)

(defvar *component-pathname*
  (asdf:component-relative-pathname (asdf:find-system "aoc-2019")))

(defun file-in-day (file day)
  (merge-pathnames
   file
   (merge-pathnames (make-pathname :directory (list :relative (format nil "day-~d" day)))
                    *component-pathname*)))

(defun read-lines (file)
  (with-open-file (input file)
    (loop
       :for line := (read-line input nil nil)
       :while line :collect line)))
