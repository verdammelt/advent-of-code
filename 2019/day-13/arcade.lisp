(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../computer")
  (load "../file-utils")
  (load "../string-utils"))

(defpackage :arcade
  (:use :common-lisp)
  (:export :play))

(in-package :arcade)

(defun load-program (file)
  (mapcar #'parse-integer
          (string-utils:split (first (file-utils:read-lines file)) #\,)))

(defun triples (seq)
  (labels ((grab (n seq acc)
             (cond ((null seq) acc)
                   ((< (length seq) n) (push seq acc))
                   (t (grab n (subseq seq n) (push (subseq seq 0 n) acc))))))
    (nreverse (grab 3 seq (list)))))

(defun parse-output (output)
  (mapcar #'parse-integer (string-utils:split (string-trim '(#\Space) output) #\Space)))

(defun play (game-file)
  (triples (parse-output (computer:get-output
                          (computer:compute (load-program game-file)
                                            :output-stream (make-string-output-stream))))))
