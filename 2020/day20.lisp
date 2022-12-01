(defpackage #:aoc-2020-20
  (:use :cl #:aoc #:aoc-2020/utils))

(in-package #:aoc-2020-20)

(aoc:def-today-suite*)

(defun parse-tile (tile)
  (let ((number (parse-integer (first tile) :start (length "Tile") :junk-allowed t))
        (data (rest tile)))
    (list number (make-array (list (length (first data)) (length data)) :initial-contents data))))

(defun read-data-file (&optional label)
  (read-data (today-data-pathname label)
             :post-process #'(lambda (lines) (mapcar #'parse-tile (aoc:split-lines-on-empty-line lines)))))

(defun make-tile (number matrix) (list number matrix))
(defun tile-number (tile) (first tile))
(defun tile-matrix (tile) (second tile))

(defparameter +example+ (read-data-file "example"))
(defparameter +input+ (read-data-file))

(defun flip-matrix-horiz (matrix)
  "Flips MATRIX around vertical axis"
  (let ((new (make-array (array-dimensions matrix)))
        (max-x (array-dimension matrix 0))
        (max-y (array-dimension matrix 1)))
    (loop for x below max-x
          do (loop for y below max-y
                   do (setf (aref new x (- max-y y 1)) (aref matrix x y))))
    new))

(defun flip-matrix-vert (matrix)
  "Flips MATRIX around horizontal axis"
  (let ((new (make-array (array-dimensions matrix)))
        (max-x (array-dimension matrix 0))
        (max-y (array-dimension matrix 1)))
    (loop for x below max-x
          do (loop for y below max-y
                   do (setf (aref new (- max-x x 1) y) (aref matrix x y))))
    new))


(defun transpose-matrix (matrix)
  (let ((new (make-array (array-dimensions matrix)))
        (max-x (array-dimension matrix 0))
        (max-y (array-dimension matrix 1)))
    (loop for x below max-x
          do (loop for y below max-y
                   do (setf (aref new y x) (aref matrix x y))))
    new))

(defun rotate-matrix-right (matrix)
  "Rotates MATRIX 90deg clockwise"
  (flip-matrix-horiz (transpose-matrix matrix)))

(defun rotate-matrix-left (matrix)
  "Rotates MATRIX 90deg counterclockwise"
  (flip-matrix-vert (transpose-matrix matrix)))

(defun print-matrix (matrix &optional (stream t))
  (loop for x below (array-dimension matrix 0)
        do (progn (loop for y below (array-dimension matrix 1)
                        do (format stream "~A" (aref matrix x y)))
                  (terpri stream)))
  (values))

(5am:def-test matrix-flip-rotate (:suite :aoc-2020-20)
  (let ((matrix #2A((1 2 3) (4 5 6) (7 8 9))))
    (5am:is (equalp #2A((3 2 1) (6 5 4) (9 8 7))
                    (flip-matrix-horiz matrix)))
    (5am:is (equalp #2A((7 8 9) (4 5 6) (1 2 3))
                    (flip-matrix-vert matrix)))

    (5am:is (equalp #2A((7 4 1) (8 5 2) (9 6 3))
                    (rotate-matrix-right matrix)))
    (5am:is (equalp #2A((3 6 9) (2 5 8) (1 4 7))
                    (rotate-matrix-left matrix)))))

(defun all-rotations-and-flips (matrix)
  (mapcar #'(lambda (fn) (funcall fn matrix))
          (list #'identity
                #'flip-matrix-vert
                #'flip-matrix-horiz
                #'rotate-matrix-left
                #'rotate-matrix-right
                #'(lambda (m) (rotate-matrix-left (flip-matrix-vert m)))
                #'(lambda (m) (rotate-matrix-right (flip-matrix-vert m)))
                #'(lambda (m) (rotate-matrix-left (flip-matrix-horiz m)))
                #'(lambda (m) (rotate-matrix-right (flip-matrix-horiz m))))))

(defun corresponding-edge (edge)
  (ecase edge
    (:top :bottom)
    (:bottom :top)
    (:left :right)
    (:right :left)))

(defun get-matrix-edge (matrix edge)
  (ecase edge
    (:left (make-array (list (array-dimension matrix 0))
                      :initial-contents
                      (loop for x below (array-dimension matrix 0)
                            collect (aref matrix x 0))))
    (:right (make-array (list (array-dimension matrix 0))
                         :initial-contents
                         (loop for x below (array-dimension matrix 0)
                               collect (aref matrix x (1- (array-dimension matrix 1))))))
    (:top (make-array (list (array-dimension matrix 1))
                       :initial-contents
                       (loop for y below (array-dimension matrix 1)
                             collect (aref matrix 0 y))))
    (:bottom (make-array (list (array-dimension matrix 1))
                        :initial-contents
                        (loop for y below (array-dimension matrix 1)
                              collect (aref matrix (1- (array-dimension matrix 0)) y))))))

(defun matrix-edge-match-p (matrix1 matrix2 edge)
  "Checks if edge of MATRIX1 matches corresponding edge of MATRIX2.
EDGE determines the edge of MATRIX1 to use."
  (equalp (get-matrix-edge matrix1 edge)
          (get-matrix-edge matrix2 (corresponding-edge edge))))

(defun place-tile (tile &key top bottom left right)
  (list tile
        (pairlis (list :top :bottom :left :right) (list top bottom left right))))

(defun empty-edges (placed-tile)
  (mapcar #'car (remove-if-not #'null (second placed-tile) :key #'cdr)))

(defun find-place-for-tile (tile placed)
  (let ((edge-tiles (remove-if-not #'empty-edges placed))
        (all-possible (all-rotations-and-flips (tile-matrix tile)))
        (found nil))
    (dolist (p edge-tiles found)
      (dolist (e (empty-edges p))
        (dolist (try all-possible)
          (when (matrix-edge-match-p (tile-matrix (first p)) try e)
            (push (list (make-tile (tile-number tile) try)
                   (tile-number (first p))
                     e) found)))
        )
      ))
)

(defun assemble-tiles (unplaced placed)
  (cond ((null placed) (assemble-tiles (cdr unplaced) (push (place-tile (car unplaced)) placed)))
        ((null unplaced) placed)
        ((= 3 (length unplaced)) (list unplaced placed))
        (t (let ((found (car (find-place-for-tile (car unplaced) placed))))
             (if found
                 (destructuring-bind (new-tile placed-tile edge) found
                     (assemble-tiles (cdr unplaced)
                                     (push (funcall #'place-tile new-tile edge placed-tile) placed)))
                 (progn
                   (assemble-tiles (append (cdr unplaced) (list (car unplaced))) placed))
               )))
        )
  )
