(defpackage #:aoc-2020-03
  (:use :cl))

(in-package #:aoc-2020-03)

(defparameter +input+ (aoc:read-data (aoc:today-data-pathname)))
(defparameter +example+ (aoc:read-data (aoc:today-data-pathname "example")))

(defun slope-run (slope) (realpart slope))
(defun slope-rise (slope) (imagpart slope))

(defun map-height (map) (length map))
(defun map-width (map) (length (first map)))
(defun map-elt (map x y) (char (nth y map) x))

(defun tree-p (x) (char= #\# x))

(defun compute-repeat (slope map)
  (let ((map-height (map-height map))
        (map-width (map-width map))
        (rise (slope-rise slope))
        (run (slope-run slope)))

    (/ (* (/ map-height rise) run) map-width)))

(defun repeat-pattern (map count)
  (mapcar #'(lambda (l) (apply #'concatenate 'string (loop for i below count collect l))) map))

(defun travel (slope map)
  (let ((map (repeat-pattern map (compute-repeat slope map)))
        (rise (slope-rise slope))
        (run (slope-run slope)))
    (loop for x from 0 by run
          for y from 0 below (map-height map) by rise
          count (tree-p (map-elt map x y)))))

(defparameter +paths+ '(#C(1 1) #C(3 1) #C(5 1) #C(7 1) #C(1 2)))

(defun travel-paths (paths map)
  (mapcar #'(lambda (slope) (travel slope map)) paths))
