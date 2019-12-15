(defpackage :string-utils
  (:use :common-lisp)
  (:export :split :split-if))

(in-package :string-utils)

(defun split-if (string predicate)
  (labels ((split-string-iter (string result)
             (let ((idx (position-if predicate string :from-end t)))
               (if idx
                   (split-string-iter (subseq string 0 idx)
                                      (cons (subseq string (1+ idx)) result))
                   (cons string result)))))
    (split-string-iter string (list))))

(defun split (string &optional (delimeter #\Space))
  (split-if string #'(lambda (c) (char= c delimeter))))
