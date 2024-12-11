(defpackage #:aoc-2024-10
  (:use :cl))

(in-package #:aoc-2024-10)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'aoc:number-string->list-of-digits
                 :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun coord-neighbors (coord)
  (let ((dirs (list (aoc:make-coord -1 0)
                    (aoc:make-coord 0 1)
                    (aoc:make-coord 1 0)
                    (aoc:make-coord 0 -1))))
    (mapcar #'(lambda (d) (aoc:coord-add coord d)) dirs)))

(defun neighbor-valid-p (map current neighbor)
  (let ((curr-height (aoc:coord-aref map current))
        (neighbor-height (or (aoc:coord-safe-aref map neighbor) most-negative-fixnum)))
    (= (1+ curr-height) neighbor-height)))

(defun compute-valid-neighbors (map coord)
  (remove-if-not #'(lambda (n) (neighbor-valid-p map coord n)) (coord-neighbors coord)))

(defun find-trailheads (map)
  (let ((trailheads (list)))
    (aoc:map-2d-array
     #'(lambda (m x y) (when (zerop (aref m x y)) (push (aoc:make-coord x y) trailheads)))
     map)
    (nreverse trailheads)))

(defun find-trails-from-head (map trailhead)
  ;; TODO: [2024-12-10] feels awkward the collected-trails separate, trouble getting it into recursive fn
  (let ((collected-trails (list)))
   (labels ((%recur (current-trail neighbors)
              (let ((curr-height (aoc:coord-aref map (first current-trail))))
                (cond ((= 9 curr-height)
                       (push (reverse current-trail) collected-trails))
                      ((not neighbors) nil)
                      (t (mapcar #'(lambda (n) (%recur (append (list n) current-trail)
                                                  (compute-valid-neighbors map n)))
                                 neighbors))))))
     (%recur (list trailhead) (compute-valid-neighbors map trailhead))
     collected-trails)))

(defun find-all-trails-on-map (map)
  (mapcar #'(lambda (th) (find-trails-from-head map th)) (find-trailheads map)))

(defun part1 (input)
  (let* ((trails (find-all-trails-on-map input))
         (unique-endpoints
           (mapcar #'(lambda (l) (remove-duplicates (mapcar #'last l) :test #'equal)) trails))
         (scores (mapcar #'length unique-endpoints)))
    (aoc:sum scores)))

(5am:def-test part1 (:suite :aoc-2024-10)
  (5am:is (= 36 (part1 +example+)))
  (5am:is (= 688 (part1 +input+))))

(defun part2 (input)
  (let ((trails (find-all-trails-on-map input)))
    (aoc:sum (mapcar #'length trails))))

(5am:def-test part2 (:suite :aoc-2024-10)
  (5am:is (= 81 (part2 +example+)))
  (5am:is (= 1459 (part2 +input+))))
