(defpackage #:aoc-2020-09
  (:use :cl #:aoc #:aoc-2020/utils))

(in-package #:aoc-2020-09)

(5am:def-suite :aoc-2020-09 :in :aoc-2020)

(defun make-xmas-cypher (window input)
  (list window input))

(defun xmas-cypher-window (cypher) (first cypher))
(defun xmas-cypher-input (cypher) (second cypher))

(defparameter +input+ (read-data (today-data)
                                 :line-parser #'parse-integer
                                 :post-process (partial #'make-xmas-cypher 25)))
(defparameter +example+ (read-data (today-data "example")
                                   :line-parser #'parse-integer
                                   :post-process (partial #'make-xmas-cypher 5)))

(defun sums-of-two (numbers)
  (mapcar #'(lambda (xy) (apply #'+ xy)) (combo-pairs numbers)))

(defun find-first-wrong (cypher)
  (let ((window-size (xmas-cypher-window cypher))
        (input (xmas-cypher-input cypher)))
    (let ((values-to-check
            (loop for idx below (- (length input) window-size)
                  collect (reverse (subseq input idx (+ idx window-size 1))))))
      (labels ((checksum (value inputs)
                 (list value (and (member value (sums-of-two inputs)) t)))
               (check-value (value-to-check) (checksum (first value-to-check)
                                                       (rest value-to-check))))
        (first (find nil (mapcar #'check-value values-to-check) :key #'second))))))

(5am:def-test part1 (:suite :aoc-2020-09)
  (5am:is (= 127 (find-first-wrong +example+)))
  (5am:is (= 105950735 (find-first-wrong +input+))))

(defun find-contigous-sum (target)
  (lambda (number-list) (labels ((%contingous-sum (nums sum acc)
                       (cond ((zerop sum) acc)
                             ((or (minusp sum)
                                  (null nums)) nil)
                             (t (%contingous-sum (rest nums) (- sum (first nums)) (push (first nums) acc))))))
              (%contingous-sum number-list target (list)))))

(defun find-weakness (cypher)
  (let ((target (find-first-wrong cypher))
        (number-list (xmas-cypher-input cypher)))
    (flet ((too-short (l) (< (length l) 2)))
      (let ((sorted-range
              (sort (find-if-not
                     #'too-short
                     (maplist (find-contigous-sum target) number-list))
                    #'<)))
        (funcall #'+
               (first sorted-range)
               (first (last sorted-range)))))))

(5am:def-test part2 (:suite :aoc-2020-09)
  (5am:is (= 62 (find-weakness +example+)))
  (5am:is (= 13826915 (find-weakness +input+))))
