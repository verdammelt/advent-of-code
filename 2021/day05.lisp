(defpackage #:aoc-2021-05
  (:use :cl))

(in-package #:aoc-2021-05)

(aoc:def-today-suite*)

(defun parse-point (str)
  (aoc:string-of-numbers->list-of-numbers str #\,))

(defun make-point (x y) (list x y))
(defun point-x (point) (first point))
(defun point-y (point) (second point))

(defun point-equal (p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(defun point-add (p1 p2)
  (make-point (+ (point-x p1) (point-x p2))
              (+ (point-y p1) (point-y p2))))

(defun parse-line (str)
  (let ((pairs-of-points (aoc:split-string-on-chars '(#\- #\> #\Space) str)))
    (mapcar #'parse-point pairs-of-points)))

(defun line-start (line) (first line))
(defun line-end (line) (second line))

(defun line-direction (line)
  "Determine the 'direction' of the LINE. This is represented as a POINT with x
and y values of [-1, 0, 1]."
  (let ((start (line-start line))
        (end (line-end line)))
    (make-point (signum (- (point-x end) (point-x start)))
                (signum (- (point-y end) (point-y start))))))

(defun horizontal-p (line)
  (apply #'= (mapcar #'point-x line)))

(defun vertical-p (line)
  (apply #'= (mapcar #'point-y line)))

(defun all-points-in-line (line)
  (let* ((start (line-start line))
         (end (line-end line))
         (delta (line-direction line)))
    (loop for p = start then (point-add p delta)
          collect p
          until (point-equal p end))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-line))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +part1+
  (read-data (aoc:today-data-pathname)))

(defun only-horiz-vert-lines (lines)
  (remove-if-not #'(lambda (l) (or (horizontal-p l) (vertical-p l))) lines))

(defun group-points (lines)
  "Produce a hashtable of POINT->NUMBER where NUMBER is the count of LINES that contain that point."
  (let ((all-points (apply #'concatenate 'list (mapcar #'all-points-in-line lines))))
    (reduce #'(lambda (h p) (incf (gethash p h 0)) h)
            all-points
            :initial-value (make-hash-table :test #'equal))))

(defun part1 (input)
  (count-if #'(lambda (n) (>= n 2))
   (alexandria:hash-table-values
    (group-points (only-horiz-vert-lines input)))))

(5am:def-test part1 (:suite :aoc-2021-05)
  (5am:is (= 5 (part1 +example+)))
  (5am:is (= 6461 (part1 +part1+))))

(defun part2 (input)
  (count-if #'(lambda (n) (>= n 2))
   (alexandria:hash-table-values
    (group-points input))))

(5am:def-test part2 (:suite :aoc-2021-05)
  (5am:is (= 12 (part2 +example+)))
  (5am:is (= 18065 (part2 +part1+))))
