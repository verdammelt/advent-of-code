(in-package #:aoc)

(defun make-coord (x y)
  (list x y))

(defun coord-x (coord) (first coord))
(defun coord-y (coord) (second coord))

(defun coord-equal (c1 c2) (equal c1 c2))

(defun coord-add (coord &rest other-coords)
  (make-coord (apply #'+ (coord-x coord) (mapcar #'coord-x other-coords))
              (apply #'+ (coord-y coord) (mapcar #'coord-y other-coords))))

(defun coord-in-bounds (map coord)
  (destructuring-bind (x-limit y-limit) (array-dimensions map)
    (and (<= 0 (coord-x coord) (1- x-limit))
         (<= 0 (coord-y coord) (1- y-limit)))))

(defun coord-aref (map coord)
  (aref map (coord-x coord) (coord-y coord)))

(defun (setf coord-aref) (new-value map coord)
  (setf (aref map (coord-x coord) (coord-y coord)) new-value))

(defun coord-safe-aref (map coord)
  (and (coord-in-bounds map coord) (coord-aref map coord)))
