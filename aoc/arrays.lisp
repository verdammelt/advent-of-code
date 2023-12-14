(in-package #:aoc)

(defun lists->2d-array (lists)
  (make-array (list (length lists) (length (first lists)))
              :initial-contents lists))

(defun map-2d-array (fn array)
  "Maps FN across each item in ARRAY creating a new array (of same diminesions)
with the results. FN is a function that takes 3 arguments: the ARRAY, the ROW
coordinate and the COLUMN coordinate."
  (let* ((new-array (make-array (array-dimensions array) :initial-element nil)))
    (loop for row from 0 below (array-dimension array 0)
          do (loop for col from 0 below (array-dimension array 1)
                   do (setf (aref new-array row col) (funcall fn array row col))))
    new-array))

(defun slice-2d-array (array direction idx)
  "Returns a ROW or COL slice of ARRAY based upon DIRECTION and IDX.
example:
(SLICE-2D-ARRAY ARRAY :ROW 3) returns the fourth row of ARRAY
(SLICE-2D-ARRAY ARRAY :COL 2) returns the third column of ARRAY"
  (declare (type (member :row :col) direction))
  (declare (type integer idx))
  (destructuring-bind (max-row max-col) (array-dimensions array)
    (let* ((max (if (eq direction :row) max-col max-row))
           (vec (make-array (list max))))
      (dotimes (n max)
        (setf (aref vec n)
              (if (eq direction :row)
                  (aref array idx n)
                  (aref array n idx))))
      vec)))

(defun print-2d-array (array &key (stream t) (draw-axis-p t))
  (destructuring-bind (max-row max-col) (array-dimensions array)
    (let ((row-idx-width (ceiling (log max-row 10))))
      (with-output-to-string (str)
        (when draw-axis-p
          (progn (format stream "~vT" (1+ row-idx-width))
                 (loop for col below max-col
                       do (princ (if (zerop (mod col 10)) #\+ #\-) stream))))
        (loop for row below max-row
              do (fresh-line stream)
              when draw-axis-p
                do (format stream "~v,' D " row-idx-width row)
              do (loop for col below (array-dimension array 1)
                       do (princ (aref array row col) stream))))))
  (values))

(defun manhattan-distance (p1 p2)
  "Compute the 'Manhattan Distance' (https://simple.wikipedia.org/wiki/Manhattan_distance)
between P1 and P2. Both points are in the form (X . Y)"
  (let ((x1 (car p1)) (y1 (cdr p1))
        (x2 (car p2)) (y2 (cdr p2)))
    (+ (abs (- x1 x2))
       (abs (- y1 y2)))))
