(defpackage #:aoc-2024-11
  (:use :cl))

(in-package #:aoc-2024-11)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'aoc:string-of-numbers->list-of-numbers
                 :post-process #'first))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun split-digits (n)
  (let* ((str (format nil "~D" n))
         (split-idx (ceiling (/ (length str) 2))))
    (list (subseq str 0 split-idx) (subseq str split-idx))))

(defun process-stone (stone)
  (let ((split-stone (split-digits stone)))
    (cond ((zerop stone) (list 1))
          ((= (length (first split-stone)) (length (second split-stone)))
           (list (parse-integer (first split-stone)) (parse-integer (second split-stone))))
          (t (list (* stone 2024))))))

(defun parse-input (input)
  (reduce #'(lambda (stones stone) (incf (gethash stone stones 0)) stones) input
          :initial-value (make-hash-table)))

(defun blink (stones)
  (let ((new-stones (make-hash-table)))
    (maphash #'(lambda (stone count)
                 (map nil #'(lambda (stone) (incf (gethash stone new-stones 0) count))
                      (process-stone stone)))
             stones)
    new-stones))

(defun part1 (input &optional (blinks 25))
  (let ((stones (parse-input input)))
    (dotimes (i blinks)
      (setf stones (blink stones)))
    (aoc:sum (alexandria:hash-table-values stones))))

(5am:def-test part1 (:suite :aoc-2024-11)
  (5am:is (= 55312 (part1 +example+)))
  (5am:is (= 199986 (part1 +input+))))

(defun part2 (input)
  (part1 input 75))

(5am:def-test part2 (:suite :aoc-2024-11)
  (5am:is (= 65601038650482 (part2 +example+)))
  (5am:is (= 236804088748754 (part2 +input+))))
