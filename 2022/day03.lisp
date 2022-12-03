(defpackage #:aoc-2022-03
  (:use :cl))

(in-package #:aoc-2022-03)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file
                                       :pre-process #'(lambda (ls) (remove-if #'aoc:empty-string-p ls))
                                       :line-parser #'(lambda (l) (coerce l 'list))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun priority (item)
  (let* ((idx (position item "abcdefghijklmnopqrstuvwxyz" :test #'char-equal))
         (lower-base 1)
         (upper-base 27))
    (+ idx (if (upper-case-p item) upper-base lower-base))))

(defun make-rucksack (cs)
  (let* ((half (/ (length cs) 2)))
    (list (subseq cs 0 half) (subseq cs half))))

(defun in-both-compartments (rucksack)
  (first (intersection (first rucksack) (second rucksack))))

(defun part1 (rucksacks)
  (aoc:sum (mapcar #'priority
                   (mapcar #'in-both-compartments
                           (mapcar #'make-rucksack rucksacks)))))

(5am:def-test part1 (:suite :aoc-2022-03)
  (5am:is (= 157 (part1 +example+)))
  (5am:is (= 8349 (part1 +input+))))

(defun by-3s (list)
  (loop for (x y z) on list by #'cdddr collect (list x y z)))

(defun group-token (group)
  (first (intersection (first group)
                       (intersection (second group) (third group)))))

(defun part2 (input)
  (aoc:sum (mapcar #'priority (mapcar #'group-token (by-3s input)))))

(5am:def-test part2 (:suite :aoc-2022-03)
  (5am:is (= 70 (part2 +example+)))
  (5am:is (= 2681 (part2 +input+))))
