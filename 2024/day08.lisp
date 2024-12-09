(defpackage #:aoc-2024-08
  (:use :cl))

(in-package #:aoc-2024-08)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun collect-antennae (map)
  (let ((antennae (make-hash-table)))
    (aoc:map-2d-array
     #'(lambda (m x y)
         (let ((item (aref m x y)))
           (when (alphanumericp item)
             (push (aoc:make-coord x y) (gethash item antennae '())))))
     map)
    antennae))

(defun coord-inverse (coord)
  (aoc:make-coord (* -1 (aoc:coord-x coord)) (* -1 (aoc:coord-y coord))))

(defun compute-distances (locations)
  "LOCATIONS is a list of coordinates. Compute the distance between every pair of coordinates.
Evaluates to a list of triples: (COORD1 COORD2 DISTANCE)."
  (aoc:flatten
   (remove nil
           (maplist
            #'(lambda (l) (let ((start (car l))
                           (rest (cdr l)))
                       (when (not (null rest))
                         (mapcar
                          #'(lambda (c1)
                              (list start c1
                                    (aoc:make-coord (- (aoc:coord-x c1)
                                                       (aoc:coord-x start))
                                                    (- (aoc:coord-y c1)
                                                       (aoc:coord-y start)))))
                          rest))))
            locations))))

(defun compute-antinodes-for-antenna (antenna-locations)
  (let ((distances (compute-distances antenna-locations)))
    (loop for (start end distance) in distances
          collect (aoc:coord-add start (coord-inverse distance))
          collect (aoc:coord-add end distance))))

(defun part1 (input)
  (let ((antennae (collect-antennae input))
        (antinodes (list)))

    (maphash #'(lambda (freqency locations)
                 (declare (ignore freqency))
                 (setf antinodes
                       (append (compute-antinodes-for-antenna locations) antinodes)))
             antennae)

    (length
     (remove-if-not
      #'(lambda (coord) (aoc:coord-in-bounds input coord))
      (remove-duplicates antinodes :test #'aoc:coord-equal)))))

(5am:def-test part1 (:suite :aoc-2024-08)
  (5am:is (= 14 (part1 +example+)))
  (5am:is (= 228 (part1 +input+))))

(defun compute-all-antinodes-for-antenna (map locations)
  (let ((distances (compute-distances locations))
        (antinodes (list)))
    (flet ((in-bounds (c) (aoc:coord-in-bounds map c)))
      (dolist (triple distances antinodes)
        (do ((start (first triple))
             (end (second triple))
             (distance (third triple)))

            ((and (null start) (null end)))

          (if (and start (in-bounds start))
              (progn (push start antinodes)
                     (setq start (aoc:coord-add start distance)))
              (setq start nil))
          (if (and end (in-bounds end))
              (progn (push end antinodes)
                     (setq end (aoc:coord-add end (coord-inverse distance))))
              (setq end nil)))))))

(defun part2 (input)
  (let ((antennae (collect-antennae input))
        (antinodes (list)))

    (maphash #'(lambda (frequency locations)
                 (declare (ignore frequency))
                 (setf antinodes
                       (append antinodes
                               (compute-all-antinodes-for-antenna input locations))))
             antennae)

    (length (remove-duplicates antinodes :test #'aoc:coord-equal))))

(5am:def-test part2 (:suite :aoc-2024-08)
  (5am:is (= 34 (part2 +example+)))
  (5am:is (= 766 (part2 +input+))))
