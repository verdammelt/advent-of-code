(defpackage :fuel-counter
  (:use :common-lisp))

(in-package :fuel-counter)

(defun fuel-cost (mass)
  (- (floor mass 3) 2))

(defun file-lines (file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
       while line collect line)))

(defun parse-line (line)
  (with-input-from-string (stream line) (read stream)))

(defun execute (input-file)
  (let* ((masses (mapcar #'parse-line (file-lines input-file)))
         (fuel-costs (mapcar #'fuel-cost masses)))
    (reduce #'+ fuel-costs)))
