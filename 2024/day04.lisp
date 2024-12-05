(defpackage #:aoc-2024-04
  (:use :cl))

(in-package #:aoc-2024-04)

(aoc:def-today-suite*)

(defun keywordize-2d-array (map)
  (aoc:map-2d-array-values #'aoc:keywordize map))

(defun read-data (file)
  (keywordize-2d-array
   (aoc:read-data
    file
    :post-process #'aoc:lists->2d-array)))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun find-all (map target)
  (loop for x below (array-dimension map 0)
        appending (loop for y below (array-dimension map 1)
                 when (eq (aref map x y) target)
                   collect (aoc:make-coord x y))))

(defun found-xmas-in-dir-p (map coord dir)
  (let ((x (aoc:coord-safe-aref map coord))
        (m (aoc:coord-safe-aref map (aoc:coord-add coord dir)))
        (a (aoc:coord-safe-aref map (aoc:coord-add coord dir dir)))
        (s (aoc:coord-safe-aref map (aoc:coord-add coord dir dir dir))))
    (and (eq x :x)
         (eq m :m)
         (eq a :a)
         (eq s :s))))

(defun part1 (input)
  (let ((starting-points (find-all input :x))
        (directions (list (aoc:make-coord 1 0)
                          (aoc:make-coord 0 1)
                          (aoc:make-coord 1 1)
                          (aoc:make-coord -1 0)
                          (aoc:make-coord 0 -1)
                          (aoc:make-coord -1 -1)
                          (aoc:make-coord -1 1)
                          (aoc:make-coord 1 -1))))
    (loop for point in starting-points
          sum (loop for dir in directions
                    count (found-xmas-in-dir-p input point dir)))))

(5AM:def-test part1 (:suite :aoc-2024-04)
  (5am:is (= 18 (part1 +example+)))
  (5am:is (= 2458 (part1 +input+))))

(defun found-x-mas-p (map coord)
  (let ((center (aoc:coord-safe-aref map coord))
        (top-left (aoc:coord-safe-aref map (aoc:coord-add coord (aoc:make-coord -1 -1))))
        (top-right (aoc:coord-safe-aref map (aoc:coord-add coord (aoc:make-coord -1 1))))
        (bottom-left (aoc:coord-safe-aref map (aoc:coord-add coord (aoc:make-coord 1 -1))))
        (bottom-right (aoc:coord-safe-aref map (aoc:coord-add coord (aoc:make-coord 1 1)))))
    (and (eq center :a)
         (or (and (eq top-left :m)
                  (eq bottom-right :s))
             (and (eq top-left :s)
                  (eq bottom-right :m)))
         (or (and (eq top-right :m)
                  (eq bottom-left :s))
             (and (eq top-right :s)
                  (eq bottom-left :m))))))

(defun part2 (input)
  (let ((starting-points (find-all input :a)))
    (loop for point in starting-points
          count (found-x-mas-p input point))))

(5am:def-test part2 (:suite :aoc-2024-04)
  (5am:is (= 9 (part2 +example+)))
  (5am:is (= 1945 (part2 +input+))))
