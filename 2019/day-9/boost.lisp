;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (load "../file-utils")
;;   (load "../string-utils")
;;   (load "../computer"))

(defpackage :boost
  (:use :common-lisp)
  (:export :boost))

(in-package :boost)

(defun csv->numbers (csv)
  (mapcar #'parse-integer (string-utils:split csv #\,)))

(defparameter *test-programs*
    (list
     (list :program (csv->numbers "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
           :expected #'(lambda (actual)
                         (assert (string= actual "109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99"))))
     (list :program (csv->numbers "1102,34915192,34915192,7,4,7,99,0")
           :expected #'(lambda (actual) (assert (= 16 (length actual)))))
     (list :program (csv->numbers "104,1125899906842624,99")
           :expected #'(lambda (actual) (assert (string= "1125899906842624" actual))))))

(defparameter *boost-program*
  (mapcar #'parse-integer
          (string-utils:split
           (first (file-utils:read-lines (file-utils:file-in-day "./input.txt" 9)))
           #\,)))

(defun boost (&optional (program *boost-program*) (input "1"))
  (string-trim '(#\Space)
               (computer:get-output
                (computer:compute program
                                  :input-stream (make-string-input-stream input)
                                  :output-stream (make-string-output-stream)))))

(dolist (p *test-programs*)
  (let ((program (getf p :program))
        (expected (getf p :expected)))
    (funcall expected (boost program))))

(assert (string= "2377080455" (boost)))
(assert (string= "74917" (boost *boost-program* "2")))

;; todo
;; * parameter mode 2 - addressing from 'global' reference base setting (initial 0)
;; * opcode 9 - setting reference base from parameter
;; x computers need more memory than length of program (maybe allocate as needed?)
;; ? refactor: computer to create op codes & update case statement at same time?
;; ? refactor: common code for parsing programs as strings (CSV splitting & parsing)
;; ? refactor: computer memory as array rather than list
;; ? refactor: remove use of get-immediate-parameter...
