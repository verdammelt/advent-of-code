(defpackage #:aoc-2023-21
  (:use :cl))

(in-package #:aoc-2023-21)

(aoc:def-today-suite*)

(defun parse-garden (lines)
  (let ((garden-array (aoc:lists->2d-array lines))
        (start-location nil)
        (rock-map (make-hash-table :test #'equal)))
    (aoc:map-2d-array
     #'(lambda (arr row col)
         (cond ((char= (aref arr row col) #\#)
                (setf (gethash (list row col) rock-map) t))
               ((char= (aref arr row col) #\S)
                (setf start-location (list row col)))))
     garden-array)
    (list start-location rock-map)))

(defun read-data (file)
  (aoc:read-data file :post-process #'parse-garden))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun neighbors-of (location)
  (destructuring-bind (row col) location
    (list (list (1- row) col)
          (list (1+ row) col)
          (list row (1- col))
          (list row (1+ col)))))

(defun filter-out-rocks (locations rock-map)
  (flet ((is-rock-p (loc) (gethash loc rock-map)))
    (remove-if #'is-rock-p locations)))

(defun walk (rock-map locations steps)
  (labels ((non-rock-neighbors (loc)
             (filter-out-rocks (neighbors-of loc) rock-map))
           (walk-iter (locations steps)
             (if (zerop steps) locations
                 (walk-iter
                  (remove-duplicates
                   (aoc:flatten (mapcar #'non-rock-neighbors locations))
                   :test #'equal)
                  (1- steps)))))
    (walk-iter locations steps)))

(defun part1 (input num-steps &optional return-locations)
  (destructuring-bind (start rocks) input
    (let ((locations (walk rocks (list start) num-steps)))
      (if return-locations (values (length locations) locations)
          (length locations)))))

(5am:def-test part1 (:suite :aoc-2023-21)
  (5am:is (= 16 (part1 +example+ 6)))
  (5am:skip ":aoc-2023-21.1 not complete")
  ;; (5am:is (= 50 (part1 +example+ 10)))
  ;; (5am:is (= 1594 (part1 +example+ 50)))
  ;; (5am:is (= 6536 (part1 +example+ 100)))
  ;; (5am:is (= 167004 (part1 +example+ 500)))
  ;; (5am:is (= 668697 (part1 +example+ 1000)))
  ;; (5am:is (= 16733044 (part1 +example+ 5000)))
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2023-21)
      (5am:skip ":aoc-2023-21.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
