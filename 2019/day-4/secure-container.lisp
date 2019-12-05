(defpackage :secure-container
  (:use :common-lisp))

(in-package :secure-container)

(defparameter *puzzle-input* '(138307 . 654504))

(defun pairs-of (seq) (map 'list #'cons seq (subseq seq 1)))

(defun tuples-of (seq n)
  (loop for i from 0 upto (- (length seq) n)
        collect (subseq seq i (+ i n))))

(defun two-but-not-three-equal-p (guess)
  (let* ((pairs (tuples-of (coerce guess 'list) 2))
         (equal-pairs (remove-if-not #'(lambda (pair) (apply #'char= pair)) pairs)))
    (remove-if #'(lambda (pair) (> (count pair equal-pairs :test #'equal) 1)) equal-pairs)))

(defun has-equal-pair-p (guess)
  (let ((pairs (tuples-of (coerce guess 'list) 2)))
    (loop for pair in pairs
          when (apply #'char= pair) collect pair)))

(defun never-decreasing-p (guess)
  (let ((pairs (pairs-of guess)))
    (notany #'(lambda (pair) (char> (car pair) (cdr pair))) pairs)))

(defun check-guess (guess)
  (and (= (length guess) 6)
       ;; (has-equal-pair-p guess)
       (two-but-not-three-equal-p guess)
       (never-decreasing-p guess)))

(defun format-guess (guess)
  (format nil "~6,'0d" guess))

(defun guess-password (input-range)
  (loop with (low . high) = input-range
        for guess from low to high
        when (check-guess (format-guess guess))
          collect guess))

;; part I correct answer: 1855
