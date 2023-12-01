(defpackage #:aoc-2021-09
  (:use :cl))

(in-package #:aoc-2021-09)

(aoc:def-today-suite*)

(defun get-height (map x y)
  "Return the height in MAP at X Y coordinate. If X or Y are outside the bounds of
the MAP returns MOST-POSITIVE-FIXNUM."
  (destructuring-bind (max-y max-x) (array-dimensions map)
    (if (or (< x 0) (< y 0)
            (>= x max-x) (>= y max-y ))
        most-positive-fixnum
        (aref map y x))))

(defun get-neighbor-coords (x y)
  (list (list (1+ x) y)
        (list (1- x) y)
        (list x (1+ y))
        (list x (1- y))))

(defun smaller-than-neighbors-p (map x y)
  (let ((height (get-height map x y))
        (neighbor-heights (mapcar #'(lambda (xy) (apply #'get-height map xy))
                                  (get-neighbor-coords x y))))
    (every #'(lambda (n) (< height n)) neighbor-heights)))

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'aoc:number-string->list-of-digits
                 :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +part1+
  (read-data (aoc:today-data-pathname)))

(defun find-low-points (map)
  (let ((low-point-map
          (aoc:map-2d-array
           #'(lambda (array row col) (smaller-than-neighbors-p array col row))
           map)))
    (loop
          for x below (array-dimension map 1)
          append (loop for y below (array-dimension map 0)
                       when (aref low-point-map y x)
                         collect (list x y)))))

(defun part1 (map)
  (let* ((low-points (find-low-points map))
         (low-heights (mapcar #'(lambda (xy) (apply #'get-height map xy)) low-points))
         (risk-levels (mapcar #'1+ low-heights)))
    (aoc:sum risk-levels)))

(5am:def-test part1 (:suite :aoc-2021-09)
  (5am:is (= 15 (part1 +example+)))
  (5am:is (= 594 (part1 +part1+))))

(defun find-basin (map basin search-list discard-list)
  (labels ((add-to-basin (xy) (push xy basin))
           (in-list-p (xy list) (member xy list :test #'equal))
           (unsearched-neighbors (xy)
             (remove-if #'(lambda (c) (or (in-list-p c basin)
                                     (in-list-p c discard-list)
                                     (in-list-p c search-list)))
                        (apply #'get-neighbor-coords xy))))
    (cond ((null search-list) basin)
          ((< (apply #'get-height map (car search-list)) 9)
           (find-basin map
                        (add-to-basin (car search-list))
                        (append (cdr search-list)
                                (unsearched-neighbors (car search-list)))
                        discard-list))
          (t (find-basin map
                          basin
                          (cdr search-list)
                          (push (car search-list) discard-list))))))

(defun find-basin-of-point (map point)
  (find-basin map (list) (list point) (list)))

(defun take (n seq) (subseq seq 0 n))

(defun part2 (map)

  (let* ((low-points (find-low-points map))
         (basins (mapcar #'(lambda (xy) (find-basin-of-point map xy)) low-points)))
  (aoc:product
   (take 3
         (sort (mapcar #'length basins) #'>)))
    ;; (reduce #'* (subseq (sort (mapcar #'length basins) #'>) 0 3))
    ))

(5am:def-test part2 (:suite :aoc-2021-09)
  (5am:is (= 1134 (part2 +example+)))
  (5am:is (= 858494 (part2 +part1+))))
