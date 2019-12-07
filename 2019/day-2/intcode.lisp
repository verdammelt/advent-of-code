(load "../file-utils")
(load "../string-utils")
(load "../computer")

(defpackage :intcode
  (:use :common-lisp))

(in-package :intcode)

(defun read-input (file)
  (mapcar #'parse-integer
          (mapcar #'(lambda (s) (string-utils:split s #\,))
                  (file-utils:read-lines file))))

(defun modify-memory (memory updates)
  (dolist (update updates memory)
    (setf (address memory (first update)) (second update))))

;; correct answer: 4945026
(defun 1202-error (input-file)
  (peek
   (compute
    (modify-memory (read-input input-file)
                   '((1 12) (2 2))))
   0))

;; target from exercise = 19690720
;; solution: (52 96)
(defun what-noun-verb-causes (raw-input target)
  (let ((possibilities
          (loop for noun from 0 upto 99
                append (loop for verb from 0 upto 99
                             collect (list (list 1 noun) (list 2 verb))))))
    (loop for possibility in possibilities
          for memory = (modify-memory (copy-seq raw-input) possibility)
          for computer = (compute memory)
          ;; do (format t "Testing ~A ~&" possibility)
          when (= target (peek computer 0))
            do (return (mapcar #'second possibility)))))
