(defpackage #:aoc-2022-05
  (:use :cl))

(in-package #:aoc-2022-05)

(aoc:def-today-suite*)

(defun parse-crates (crates)
  (let ((crates-data (reverse (butlast crates)))
        (column-data (first (last crates))))
    (let ((col-pos (loop for col from 0 below (count-if #'digit-char-p column-data)
                         collect (+ 1 (* 4 col)))))
      (reduce #'(lambda (stacks row)
                  (loop for col from 0 below (length col-pos)
                        for crate = (char row (nth col col-pos))
                        when (char/= #\Space crate)
                          do (push crate (nth col stacks)))
                  stacks)
              crates-data :initial-value (make-list (length col-pos))))))

(defun parse-instruction (instruction)
  (let ((parts (aoc:split-string-on-char #\Space instruction)))
    (flet ((number-or-keyword (str) (or (parse-integer str :junk-allowed t)
                                        (aoc:keywordize str))))
      (mapcar #'number-or-keyword parts))))

(defun parse-instructions (instructions)
  (mapcar #'parse-instruction instructions))

(defun parse-crates-and-instructions (crates-and-instructions)
  (list (parse-crates (first crates-and-instructions))
        (parse-instructions (second crates-and-instructions))))

(defun read-data (file) (aoc:read-data file :pre-process #'aoc:split-lines-on-empty-line
                                       :post-process #'parse-crates-and-instructions))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defparameter +extra+
  (read-data (aoc:today-data-pathname "extra")))

(defun top-of-stacks (stacks)
  (coerce (mapcar #'first stacks) 'string))

(defun get-crate (stacks from)
  (pop (nth (1- from) stacks)))
(defun put-crate (stacks to crate)
  (push crate (nth (1- to) stacks)))

(defun move-one-crate (stacks from to)
  (let ((crate (get-crate stacks from)))
    (put-crate stacks to crate)
    stacks))

(defun move-crate-9000 (stacks instruction)
  "Use the CraterMover-9000 to follow an instruction for moving crates.
NOTE: can only more one crate at a time."
  (let ((num (getf instruction :move))
        (from (getf instruction :from))
        (to (getf instruction :to)))
    (loop for i below num
          for stks = (move-one-crate (copy-tree stacks) from to)
            then (move-one-crate (copy-tree stks) from to)
          finally (return stks))))

(defun perform-instructions (stacks instructions move-crate)
  (reduce #'(lambda (stks instr) (funcall move-crate stks instr))
          instructions :initial-value stacks)  )

(defun simulate-crate-mover (input crate-mover)
  (let ((stacks (first input))
        (instructions (second input)))
    (top-of-stacks (perform-instructions stacks instructions crate-mover))))

(defun part1 (input)
  (simulate-crate-mover input #'move-crate-9000))

(5am:def-test part1 (:suite :aoc-2022-05)
  (5am:is (string= "CMZ" (part1 +example+)))
  (5am:is (string= "BZLVHBWQF" (part1 +input+)))
  (5am:is (string= "CHRISTMAS" (part1 +extra+))))

(defun move-many-crates (stacks num from to)
  (let ((stks (copy-tree stacks)))
    (let ((crates (loop for i below num
                        collect (get-crate stks from) into crates
                        finally (return (reverse crates)))))
      (loop for crate in crates do (put-crate stks to crate))
      stks)))

(defun move-crate-9001 (stacks instruction)
  "Use the CrateMaster-9001 to follow an instruction for moving crates.
NOTE: can move multiple crates at a time"
  (let ((num (getf instruction :move))
        (from (getf instruction :from))
        (to (getf instruction :to)))
    (move-many-crates stacks num from to)))

(defun part2 (input)
  (simulate-crate-mover input #'move-crate-9001))

(5am:def-test part2 (:suite :aoc-2022-05)
  (5am:is (string= "MCD" (part2 +example+)))
  (5am:is (string= "TDGJQTZSL" (part2 +input+)))
  (5am:is (string= "GREETINGS" (part2 +extra+))))
