(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../file-utils.fasl"))

(defpackage :ftt
  (:use :common-lisp)
  (:export :load-signals :run-n-phases :write-signal))

(in-package :ftt)

(defparameter *base-pattern* '(0 1 0 -1))

(defun repeat (n item)
  (loop for i from 0 below n
       collect item))

(defun ones-only (n) (mod n 10))

(defun compute-pattern (digit-num target-length)
  (labels ((compute (digits acc)
             (cond ((> (length acc) target-length) acc)
                   ((null digits) (compute *base-pattern* acc))
                   (t (compute (rest digits)
                               (append acc (repeat digit-num
                                                   (first digits))))))))
    (rest (compute *base-pattern* (list)))))

(defun sum-nums (nums) (apply #'+ nums))

(defun compute-phase (input)
  (let* ((input-length (length input))
         (inputs (repeat input-length input))
         (patterns (loop for i from 1 upto input-length
                      collect (compute-pattern i input-length))))
    (mapcar #'ones-only
            (mapcar #'abs
                    (mapcar #'sum-nums
                            (mapcar #'(lambda (i p) (mapcar #'* i p)) inputs patterns))))))

(defun load-signal (file)
  (mapcar #'digit-char-p (coerce (first (file-utils:read-lines file)) 'list)))

(defun run-n-phases (signal-data num-phases)
  (loop
     for input = signal-data then (compute-phase input)
     for num from 0 below num-phases
     finally (return input)))

(defun write-signal (signal-data &optional (stream t))
  (format stream "~{~D~}" signal-data))
