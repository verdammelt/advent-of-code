(defpackage #:aoc-2024-12
  (:use :cl))

(in-package #:aoc-2024-12)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun all-coords-for-map (map)
  (destructuring-bind (max-x max-y) (array-dimensions map)
    (loop for x below max-x
          appending (loop for y below max-y
                          collect (aoc:make-coord x y)))))

(defun plant-equal (p1 p2) (and p1 p2 (char= p1 p2)))


(defun gather-regions (map perimeter-count-fn)
  (let ((regions (list))
        (seen (make-hash-table :test #'equalp))) ;; PLOT -> T
    (labels ((find-same-plant-neighbors (target-plant plot)
               (loop for n in (list (aoc:coord-add plot (aoc:make-coord -1 0))
                                    (aoc:coord-add plot (aoc:make-coord 0 1))
                                    (aoc:coord-add plot (aoc:make-coord 1 0))
                                    (aoc:coord-add plot (aoc:make-coord 0 -1)))
                     when (plant-equal target-plant
                                       (aoc:coord-safe-aref map n))
                       collect n))
             (process-plot (target-plant plot)
               "from https://git.thomasvoss.com/advent-of-code/tree/2024/12/puzzles.lisp"
               (when (gethash plot seen) (return-from process-plot (list 0 0)))

               (setf (gethash plot seen) t)

               (when (not (plant-equal target-plant
                                       (aoc:coord-safe-aref map plot)))
                 (return-from process-plot (list 0 0)))

               (let ((neighbors
                       (find-same-plant-neighbors target-plant plot)))
                 (loop with area = 1
                       with perimeter = (funcall perimeter-count-fn map plot neighbors)
                       for neighbor in neighbors
                       do (destructuring-bind (a1 p1)
                              (process-plot target-plant neighbor)
                            (incf area a1)
                            (incf perimeter p1))
                       finally (return (list area perimeter))))))

      (aoc:map-2d-array
       #'(lambda (m x y)
           (let* ((plot (aoc:make-coord x y))
                  (plant (aoc:coord-safe-aref m plot))
                  (area-and-perimeter (process-plot plant plot)))
             (unless (equal '(0 0) area-and-perimeter)
               (push (list plant area-and-perimeter) regions))))
       map))
    (nreverse regions)))

(defun simple-perimeter-count (map plot neighbors)
  (declare (ignore map))
  (declare (ignore plot))
  (- 4 (length neighbors)))

(defun part1 (input)
  (let ((regions (gather-regions input #'simple-perimeter-count)))
    ;; REGIONS is (PLANT (AREA PERIMETER))
    (aoc:sum (mapcar #'aoc:product (mapcar #'second regions)))))

(5am:def-test part1 (:suite :aoc-2024-12)
  (5am:is (= 1930 (part1 +example+)))
  (5am:is (= 1533024 (part1 +input+))))

(defun corner-count (map plot neighbors)
  "from https://git.thomasvoss.com/advent-of-code/tree/2024/12/puzzles.lisp"
  (case (length neighbors)
    (0 4)
    (1 2)
    (2 (let ((left (first neighbors))
             (right (second neighbors)))
         (cond ((or (= (aoc:coord-x left)
                       (aoc:coord-x right)
                       (aoc:coord-x plot))
                    (= (aoc:coord-y left)
                       (aoc:coord-y right)
                       (aoc:coord-y plot)))
                0)
               ((plant-equal (aoc:coord-safe-aref map plot)
                             (aoc:coord-safe-aref
                              map
                              (aoc:make-coord
                               (if (= (aoc:coord-x left) (aoc:coord-x plot))
                                   (aoc:coord-x right) (aoc:coord-x left))
                               (if (= (aoc:coord-y left) (aoc:coord-y plot))
                                   (aoc:coord-y right) (aoc:coord-y left)))))
                1)
               (t 2))))
    (3 (let ((wart (loop for n in neighbors
                         if (/= (aoc:coord-x n) (aoc:coord-x plot))
                           collect n into odd-one-out-x
                         else
                           collect n into odd-one-out-y
                         finally (return (first (if (= 1 (length odd-one-out-x))
                                                    odd-one-out-x
                                                    odd-one-out-y))))))
         (loop for n in neighbors
               count (not (or (equal n wart)
                              (plant-equal
                               (aoc:coord-safe-aref map plot)
                               (aoc:coord-safe-aref
                                map
                                (aoc:make-coord
                                 (if (= (aoc:coord-x n)
                                        (aoc:coord-x plot))
                                     (aoc:coord-x wart)
                                     (aoc:coord-x n))
                                 (if (= (aoc:coord-y n)
                                        (aoc:coord-y plot))
                                     (aoc:coord-y wart)
                                     (aoc:coord-y n))))))))) )
    (4 (loop for n in (list (aoc:coord-add plot (aoc:make-coord -1 -1))
                            (aoc:coord-add plot (aoc:make-coord -1 1))
                            (aoc:coord-add plot (aoc:make-coord 1 -1))
                            (aoc:coord-add plot (aoc:make-coord 1 1)))
             count (not (plant-equal (aoc:coord-safe-aref map n)
                                     (aoc:coord-safe-aref map plot)))))))


(defun part2 (input)
  (let ((regions (gather-regions input #'corner-count)))
    ;; REGIONS is (PLANT (AREA PERIMETER))
    (aoc:sum (mapcar #'aoc:product (mapcar #'second regions)))))

(5am:def-test part2 (:suite :aoc-2024-12)
  (5am:is (= 1206 (part2 +example+)))
  (5am:is (= 910066 (part2 +input+))))
