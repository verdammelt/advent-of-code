(defpackage #:aoc-2023-11
  (:use :cl))

(in-package #:aoc-2023-11)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

;; TODO: [2023-12-11] promote to helper functions for matrix
(defun array-row (array idx)
  "Returns a vector which is row IDX of ARRAY"
  (loop for col below (array-dimension array 1)
        collect (aref array idx col) into result
        finally (return (coerce result 'vector))))

;; TODO: [2023-12-11] promote to helper functions for matrix
(defun array-col (array idx)
  "Returns a vector which is COL IDX of ARRAY"
  (loop for row below (array-dimension array 0)
        collect (aref array row idx) into result
        finally (return (coerce result 'vector))))

(defun empty-space-p (c)
  (char= c #\.))

(defun galaxy-p (c)
  (char= c #\#))

(defun find-galaxies (universe)
  (let (stars)
    (aoc:map-2d-array
     #'(lambda (array row col)
         (when (galaxy-p (aref array row col))
           (push (cons row col) stars)))
     universe)
    stars))

;; TODO: [2023-12-11] promote to helper functions for matrix
(defun manhattan-distance (p1 p2)
  (let ((x1 (car p1)) (y1 (cdr p1))
        (x2 (car p2)) (y2 (cdr p2)))
    (+ (abs (- x1 x2))
       (abs (- y1 y2)))))

(defun find-empty-rows-cols (universe)
  "Returns  ((:ROW ROW-IDX)* (:COL COL-IDX)*)"
  (loop for row-idx below (array-dimension universe 0)
        for col-idx below (array-dimension universe 1)
        when (every #'empty-space-p (array-row universe row-idx))
          collect (list :row row-idx)
        when (every #'empty-space-p (array-col universe col-idx))
          collect (list :col col-idx)))

(defun count-crossings-of-the-void (galaxies empty-space)
  "EMPTY_SPACE is a list of zero or more (:ROW ROW-IDX) items and zero or more (:COL COL-IDX) items."
  (let ((sorted-rows (sort (mapcar #'car galaxies) #'<))
        (sorted-cols (sort (mapcar #'cdr galaxies) #'<)))
    (reduce
     #'(lambda (count space)
         (cond ((and (eq (first space) :row)
                     (< (first sorted-rows) (second space) (second sorted-rows)))
                (1+ count))
               ((and (eq (first space) :col)
                     (< (first sorted-cols) (second space) (second sorted-cols)))
                (1+ count))
               (t count)))
     empty-space
     :initial-value 0)))

(defun sum-distances (galaxies empty-spaces space-multiplier)
  (let ((total 0))
    (alexandria:map-combinations
     #'(lambda (c)
         (destructuring-bind (p1 p2) c
           (incf total (manhattan-distance p1 p2))
           (incf total (* (count-crossings-of-the-void c empty-spaces)
                          (1- space-multiplier)))))
     galaxies
     :length 2)
    total))

(defun part1 (input)
  (sum-distances (find-galaxies input) (find-empty-rows-cols input) 2))

(5am:def-test part1 (:suite :aoc-2023-11)
  (5am:is (= 374 (part1 +example+)))
  (5am:is (= 9521550 (part1 +input+))))


(defun part2 (input &optional (space-multiplier 1000000))
  (sum-distances (find-galaxies input) (find-empty-rows-cols input) space-multiplier))

(5am:def-test part2 (:suite :aoc-2023-11)
  ;; part2 is equiv to part1 if empty space is doubled
  (5am:is (= 374 (part2 +example+ 2)))
  (5am:is (= 9521550 (part2 +input+ 2)))

  (5am:is (= 1030 (part2 +example+ 10)))
  (5am:is (= 8410 (part2 +example+ 100)))

  (5am:is (= 82000210 (part2 +example+)))
  (5am:is (= 298932923702 (part2 +input+))))
