(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../file-utils")
  (load "../string-utils")
  (load "../computer"))

(defpackage :repair-droid
  (:use :common-lisp)
  (:export :run-droid))

(defun load-program-file (file)
  (mapcar #'parse-integer
          (mapcar #'(lambda (s) (string-utils:split s #\,))
                  (first (file-utils:read-lines file)))))
