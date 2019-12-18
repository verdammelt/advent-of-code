(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../computer")
  (load "../string-utils")
  (load "../file-utils"))

(defpackage :flare
  (:use :common-lisp))

(in-package :flare)

(defun split-on (char) (lambda (str) (string-utils:split str char)))

(defun load-program (file)
  (mapcar #'parse-integer
          (funcall (split-on #\,)
           (first (file-utils:read-lines file)))))

(defun make-map (output)
  (let* ((raw-data (map 'vector #'code-char
                        (mapcar #'parse-integer
                                (funcall (split-on #\Space)
                                         (string-trim '(#\Space) output)))))
         (num-lines (1- (count #\Newline raw-data)))
         (line-length (/ (1- (length raw-data)) num-lines)))
    (make-array (list num-lines line-length) :displaced-to raw-data)))

(defun alignment-parameters (program)
  (let* ((computer (computer:make-computer program
                                          (make-string-input-stream "")
                                          (make-string-output-stream)))
         (output (computer:get-output (computer:run-program computer))))
    (make-map output)))

(defun print-map (map &optional (stream nil))
  (format stream "~%~{~A~}" (mapcar #'code-char map)))
