(defpackage #:aoc-2015-08
  (:use :cl))

(in-package #:aoc-2015-08)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun size-of-string (str)
  "Return the size of the string STR in memory."
  (do ((idx 0 (incf idx))
       (size 0)
       (escaped-p nil))
      ((>= idx (length str)) size)
    (let ((ch (char str idx)))
      (case ch
        (#\" ) ;; skip
        (#\\ (let ((next-ch (char str (1+ idx))))
               (incf size)
               (if (char= next-ch #\x) (incf idx 3) (incf idx 1))))
        (t (incf size))))))

(defun part1 (input)
  (- (aoc:sum (mapcar #'length input))
     (aoc:sum (mapcar #'size-of-string input))))

(5am:def-test part1 (:suite :aoc-2015-08)
  (5am:is (= 12 (part1 +example+)))
  (5am:is (= 1371 (part1 +input+))))

(defun escaped-size-of-string (str)
  "Determine the size of STR after fully escaping it"
  (length (format nil "~W" str)))

(defun part2 (input)
  (- (aoc:sum (mapcar #'escaped-size-of-string input))
     (aoc:sum (mapcar #'length input))))

(5am:def-test part2 (:suite :aoc-2015-08)
  (5am:is (= 19 (part2 +example+)))
  (5am:is (= 2117 (part2 +input+))))
