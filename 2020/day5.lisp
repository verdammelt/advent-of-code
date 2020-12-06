(defpackage #:aoc-2020-05
  (:use :cl))

(in-package #:aoc-2020-05)

(defun binary-partition (input left-char right-char)
  (do* ((idx 0 (1+ idx))
        (min 0)
        (max (1- (expt 2 (length input)))))
       ((= idx (length input)) min)

    (let ((half-range (ceiling (/ (- max min) 2))))
      (cond ((char= (char input idx) left-char)  (decf max half-range))
            ((char= (char input idx) right-char) (incf min half-range))))))

(assert (= 5 (binary-partition "RLR" #\L #\R)))
(assert (= 44 (binary-partition "FBFBBFF" #\F #\B)))

(defun seat-id (boarding-pass)
  (let ((row (binary-partition (subseq boarding-pass 0 7) #\F #\B))
        (column (binary-partition (subseq boarding-pass 7) #\L #\R)))

    (+ (* row 8) column)))

(assert (= 567 (seat-id "BFFFBBFRRR")))
(assert (= 119 (seat-id "FFFBBBFRRR")))
(assert (= 820 (seat-id "BBFFBBFRLL")))

(defparameter +input+ (aoc:read-data (aoc:data-pathname "day5" "txt")))

(defun empty-seats (boarding-passes)
  (let ((seat-ids (sort (mapcar #'seat-id boarding-passes) #'>)))
    (loop for seat from (first seat-ids) downto (first (last seat-ids))
          when (not (find seat seat-ids)) collect seat)))

(assert (equal '(623) (empty-seats +input+)))
