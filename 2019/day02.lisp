(defpackage #:aoc-2019-02
  (:use :cl))

(in-package #:aoc-2019-02)

(aoc:def-today-suite*)

(defun read-input (file)
  (aoc:string-of-numbers->list-of-numbers (car (aoc:read-data file)) #\,))

(defun modify-memory (memory one-two)
  (setf (subseq memory 1 3) one-two)
  memory)

(defun 1202-error (input-file)
  (computer:peek
   (computer:compute
    (modify-memory (read-input input-file) '(12 2)))
   0))

(5am:def-test part1 (:suite :aoc-2019-02)
  (5am:is (= 8017076 (1202-error (aoc:today-data-pathname)))))

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

(5am:def-test part2 (:suite :aoc-2019-02)
  (5am:is (equal "3146"
                 (combine-noun-verb
                  (what-noun-verb-causes (read-input (aoc:today-data-pathname))
                                         19690720)))))
