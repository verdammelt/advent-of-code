(defpackage #:aoc-2024-03
  (:use :cl))

(in-package #:aoc-2024-03)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file :post-process #'(lambda (ls) (apply #'concatenate 'string ls))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +example-2+
  (read-data (aoc:today-data-pathname "example2")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun extract-muls (str)
  (cl-ppcre:all-matches-as-strings
   "mul\\\(\\d+,\\d+\\\)"
   str))

(defun extract-number-pairs (muls)
  (mapcar #'(lambda (l) (mapcar #'parse-integer l))
          (mapcar
           #'(lambda (s)
               (aoc:split-string-on-char
                #\,
                (remove-if-not #'(lambda (c) (or (digit-char-p c) (char= c #\,))) s)))
           muls)) )

(defun part1 (input)
  (let* ((just-the-muls (extract-muls input))
         (just-the-numbers (extract-number-pairs just-the-muls)))
    (aoc:sum (mapcar #'aoc:product just-the-numbers))))

(5am:def-test part1 (:suite :aoc-2024-03)
  (5am:is (= 161 (part1 +example+)))
  (5am:is (= 189600467 (part1 +input+))))

(defun extract-instructions (str)
  (cl-ppcre:all-matches-as-strings
   "do\\\(\\\)\|don't\\\(\\\)\|mul\\\(\\d+,\\d+\\\)"
   str))

(defun process-conditionals (instructions)
  (cdr
   (reduce
    #'(lambda (state inst)
        (cond ((string= inst "don't()") (cons :dont (cdr state)))
              ((string= inst "do()") (cons :do (cdr state)))
              ((eq (car state) :do)  (cons :do (push inst (cdr state))))
              (t  state)))
    instructions
    :initial-value (cons :do nil))))

(defun part2 (input)
  (let ((instructions (extract-instructions input)))
    (aoc:sum
     (mapcar #'aoc:product
             (extract-number-pairs (process-conditionals instructions))))))

(5am:def-test part2 (:suite :aoc-2024-03)
  (5am:is (= 161 (part2 +example+)))
  (5am:is (= 48 (part2 +example-2+)))
  (5am:is (= 107069718 (part2 +input+))))
