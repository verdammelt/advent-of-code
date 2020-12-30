(defpackage #:aoc-2020-05
  (:use :cl))

(in-package #:aoc-2020-05)

(aoc:def-today-suite*)

(defun binary-partition (input left-char right-char)
  (do* ((idx 0 (1+ idx))
        (min 0)
        (max (1- (expt 2 (length input)))))
       ((= idx (length input)) min)

    (let ((half-range (ceiling (/ (- max min) 2))))
      (cond ((char= (char input idx) left-char)  (decf max half-range))
            ((char= (char input idx) right-char) (incf min half-range))))))

(5am:def-test binary-partition (:suite :aoc-2020-05)
  (5am:is (= 5 (binary-partition "RLR" #\L #\R)))
  (5am:is (= 44 (binary-partition "FBFBBFF" #\F #\B))))

(defun seat-id (boarding-pass)
  (let ((row (binary-partition (subseq boarding-pass 0 7) #\F #\B))
        (column (binary-partition (subseq boarding-pass 7) #\L #\R)))

    (+ (* row 8) column)))

(5am:def-test seat-id (:suite :aoc-2020-05)
  (5am:is (= 567 (seat-id "BFFFBBFRRR")))
  (5am:is (= 119 (seat-id "FFFBBBFRRR")))
  (5am:is (= 820 (seat-id "BBFFBBFRLL"))))

(defparameter +input+ (aoc:read-data (aoc:today-data-pathname)))

(defun empty-seats (boarding-passes)
  (let ((seat-ids (sort (mapcar #'seat-id boarding-passes) #'>)))
    (loop for seat from (first seat-ids) downto (first (last seat-ids))
          when (not (find seat seat-ids)) collect seat)))

(5am:def-test empty-seats (:suite :aoc-2020-05)
  (5am:is (equal '(623) (empty-seats +input+))))
