;; (load "../file-utils")
;; (load "../string-utils")
;; (load "../computer")

(defpackage :intcode
  (:use :common-lisp))

(in-package :intcode)

(defun read-input (file)
  (mapcar #'parse-integer
          (string-utils:split
           (car (file-utils:read-lines (file-utils:file-in-day file 2)))
           #\,)))

(defun modify-memory (memory one-two)
  (setf (subseq memory 1 3) one-two)
  memory)

(defun 1202-error (input-file)
  (computer:peek
   (computer:compute
    (modify-memory (read-input input-file) '(12 2)))
   0))

(assert (= 8017076 (1202-error "./input.txt")))

;; target from exercise = 19690720
;; solution: (52 96)
(defun what-noun-verb-causes (raw-input target)
  (let ((possibilities
          (loop for noun from 0 upto 99
                append (loop for verb from 0 upto 99
                             collect (list noun verb)))))
    (loop for possibility in possibilities
          for memory = (modify-memory (copy-seq raw-input) possibility)
          for computer = (computer:compute memory)
          ;; do (format t "Testing ~A ~&" possibility)
          when (= target (computer:peek computer 0))
            do (return possibility))))

(defun combine-noun-verb (noun-verb)
  (format nil "~{~2,'0D~}" noun-verb))

(assert (equal "3146"
               (combine-noun-verb
                (what-noun-verb-causes (read-input "./input.txt") 19690720))))
