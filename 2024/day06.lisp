(defpackage #:aoc-2024-06
  (:use :cl))

(in-package #:aoc-2024-06)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun copy-map (map)
  (aoc:map-2d-array-values #'identity map))

(defclass guard ()
  ((location :initarg :location :reader guard-location)
   (direction :initarg :direction :reader guard-direction)))

(defun make-guard (location direction)
  (make-instance 'guard :location location :direction direction))

(defmethod print-object ((obj guard) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~C ~A" (guard-figure obj) (guard-location obj))))

(defmethod guard-figure ((obj guard))
  (let ((direction (guard-direction obj)))
   (cond ((aoc:coord-equal direction (aoc:make-coord -1 0)) #\^)
         ((aoc:coord-equal direction (aoc:make-coord 1 0)) #\v)
         ((aoc:coord-equal direction (aoc:make-coord 0 -1)) #\<)
         ((aoc:coord-equal direction (aoc:make-coord 0 1)) #\>))))

(defun find-guard (map)
  (aoc:map-2d-array
   #'(lambda (m x y) (when (char= #\^ (aref m x y))
                  (return-from find-guard (aoc:make-coord x y))))
   map))

(defun guard-on-map-p (map guard)
  (aoc:coord-in-bounds map (guard-location guard)))

(defun mark-map (map coord marker)
  (when (aoc:coord-in-bounds map coord)
    (setf (aoc:coord-aref map coord) marker)))

(defun at-barrier-p (map guard)
  (let* ((location (guard-location guard))
         (direction (guard-direction guard))
         (next-location (aoc:coord-add location direction)))
    (and (aoc:coord-in-bounds map next-location)
         (or (char= #\# (aoc:coord-aref map (aoc:coord-add location direction)))
             (char= #\O (aoc:coord-aref map (aoc:coord-add location direction)))))))

(defun turn-guard (guard)
  (let* ((new-direction
           (cond ((aoc:coord-equal (guard-direction guard) (aoc:make-coord 1 0))
                  (aoc:make-coord 0 -1))
                 ((aoc:coord-equal (guard-direction guard) (aoc:make-coord 0 1))
                  (aoc:make-coord 1 0))
                 ((aoc:coord-equal (guard-direction guard) (aoc:make-coord -1 0))
                  (aoc:make-coord 0 1))
                 ((aoc:coord-equal (guard-direction guard) (aoc:make-coord 0 -1))
                  (aoc:make-coord -1 0)))))
    (setf (slot-value guard 'direction) new-direction)))

(defun move-guard (guard)
  (setf (slot-value guard 'location)
        (aoc:coord-add (guard-location guard) (guard-direction guard))))

(defun copy-guard (guard)
  (make-guard (guard-location guard) (guard-direction guard)))

(defun guard-equal (guard1 guard2)
  (and (aoc:coord-equal (guard-location guard1) (guard-location guard2))
       (aoc:coord-equal (guard-direction guard1) (guard-direction guard2))))

(defun get-guard-path (map)
  (nreverse
   (remove-duplicates
    (do ((guard (make-guard (find-guard map) (aoc:make-coord -1 0)))
         (map (copy-map map))
         (visited-locations (list)))
        ((not (guard-on-map-p map guard)) visited-locations)
      ;; (mark-map map (guard-location guard) #\X)
      (push (guard-location guard) visited-locations)
      (if (at-barrier-p map guard)
          (turn-guard guard)
          (move-guard guard))
      ;; (mark-map map (guard-location guard) (guard-figure guard))
      )
    :test #'equal)))

(defun part1 (input)
  (length (get-guard-path input)))

(5am:def-test part1 (:suite :aoc-2024-06)
  (5am:is (= 41 (part1 +example+)))
  (5am:is (= 5516 (part1 +input+))))

(defun guard-has-looped-p (guard past-positions)
  (find guard past-positions :test #'guard-equal))

(defun check-if-guard-loops-on-map (map guard)
  (do ((guard (copy-guard guard))
       (past-positions (list)))
      ((or (not (guard-on-map-p map guard))
           (guard-has-looped-p guard past-positions))
       (guard-has-looped-p guard past-positions))
    ;; (mark-map map (guard-location guard) #\X)
    (push (copy-guard guard) past-positions)
    (if (at-barrier-p map guard)
        (turn-guard guard)
        (move-guard guard))
     ;; (mark-map map (guard-location guard) (guard-figure guard))
    ))

(defun part2 (input)
  (let ((current-path (get-guard-path input))
        (initial-guard (make-guard (find-guard input) (aoc:make-coord -1 0)))
        (count-of-maps-with-loops 0))
    (dolist (possible-barrier
             (remove (guard-location initial-guard) current-path :test #'aoc:coord-equal))
      (let ((map (copy-map input))
            (guard (copy-guard initial-guard)))
        ;; add a new barrier
        (mark-map map possible-barrier #\O)

        ;; see if guard goes into a loop or walks off map like normal
        (when (check-if-guard-loops-on-map map guard)
          (incf count-of-maps-with-loops))))
    count-of-maps-with-loops))

(5am:def-test part2 (:suite :aoc-2024-06)
  (5am:is (= 6 (part2 +example+)))
  (5am:is (= 2008 (part2 +input+))))
