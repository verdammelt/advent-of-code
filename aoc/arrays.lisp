(in-package #:aoc)

(defun lists->2d-array (lists)
  (make-array (list (length lists) (length (first lists)))
              :initial-contents lists))

(defun map-2d-array (fn array)
  "Maps FN across each item in ARRAY creating a new array (of same diminesions)
with the results. FN is a function that takes 3 arguments: the ARRAY, the ROW
coordinate and the COLUMN coordinate."
  (let* ((new-array (make-array (array-dimensions array) :initial-element nil)))
    (loop for y from 0 below (array-dimension array 0)
          do (loop for x from 0 below (array-dimension array 1)
                   do (setf (aref new-array y x) (funcall fn array x y))))
    new-array))
