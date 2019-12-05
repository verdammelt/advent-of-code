(defpackage :secure-container
  (:use :common-lisp))

(in-package :secure-container)

(defparameter *puzzle-input* '(138307 . 654504))

(defun pairs-of (seq) (map 'list #'cons seq (subseq seq 1)))

(defun has-equal-pair-p (guess)
  (let ((pairs (pairs-of guess)))
    (some #'(lambda (pair) (char= (car pair) (cdr pair))) pairs)))

(defun never-decreasing-p (guess)
  (let ((pairs (pairs-of guess)))
    (notany #'(lambda (pair) (char> (car pair) (cdr pair))) pairs)))

(defun check-guess (guess)
  (and (= (length guess) 6)
       (has-equal-pair-p guess)
       (never-decreasing-p guess)))

(defun format-guess (guess)
  (format nil "~6,'0d" guess))

(defun guess-password (input-range)
  (loop with (low . high) = input-range
        for guess from low to high
        when (check-guess (format-guess guess))
          collect guess))

;; part I correct answer: 1855
