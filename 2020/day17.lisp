(defpackage #:aoc-2020-17
  (:use :cl #:aoc-2020/utils))

(in-package #:aoc-2020-17)

(aoc:def-today-suite*)

(defun parse-world (lines)
  "parse the lines into active coordinates of a world."
  (let ((array (make-array (list (length lines) (length (first lines)))
                           :initial-contents lines)))
    (do ((coords (list))
         (y 0 (1+ y)))
        ((>= y (array-dimension array 0)) (nreverse coords))
      (do ((x 0 (1+ x)))
          ((>= x (array-dimension array 1)))
        (when (char= (aref array y x) #\#)
          (push (list x y 0) coords))))))

(defun read-data (&optional label)
  (aoc:read-data (aoc:today-data-pathname label)
                 :post-process #'parse-world))

(defparameter +example+ (read-data "example"))
(defparameter +input+ (read-data))

(defun make-world (&optional active-coords)
  (reduce #'(lambda (world coord) (activate world coord) world)
          active-coords :initial-value (make-hash-table :test #'equal)))

(defun active-p (world coord)
  (gethash coord world))

(defun active-coords (world)
  (alexandria:hash-table-keys world))

(defun activate (world coord)
  (setf (gethash coord world) t)
  world)

(defun inactivate (world coord)
  (remhash coord world)
  world)

(defun print-world (world)
  (let* ((active (active-coords world))
         (mins (reduce #'(lambda (c1 c2) (mapcar #'min c1 c2)) active))
         (maxs (reduce #'(lambda (c1 c2) (mapcar #'max c1 c2)) active))
         (range (mapcar #'1+ (mapcar #'- maxs mins))))
    (dotimes (z (third range))
      (format t "z = ~D~&" z)
      (let ((this-slice (remove-if-not #'(lambda (c) (= z (third c))) active)))
        (format t "range: ~S - ~S~&"
                (reduce #'(lambda (c1 c2) (mapcar #'min c1 c2)) active)
                (reduce #'(lambda (c1 c2) (mapcar #'max c1 c2)) active))
        (format t "~S" this-slice)))))

(defun apply-offset-to-coords (offset dim coords)
  (flet ((apply-offset (coord)
           (let ((new (copy-seq coord)))
             (incf (elt new dim) offset)
             new)))
    (mapcar #'apply-offset coords)))

(defun neighbors-of (coord)
  (do ((d 0 (1+ d))
       (offsets '(-1 0 1))
       (neighbors (list coord)))
      ((>= d (length coord)) (remove coord neighbors :test #'equal))
    (let ((new (make-list (length offsets) :initial-element neighbors))
          (dim-list (make-list (length offsets) :initial-element d)))
      (setf neighbors (flatten
                       (mapcar #'apply-offset-to-coords
                               offsets
                               dim-list
                               new))))))

(5am:def-test neighbors-of (:suite :aoc-2020-17)
  (5am:is (equal '((-1) (1)) (neighbors-of '(0))))
  (5am:is (equal '((-1 -1) (+0 -1) (+1 -1)
                   (-1 +0)         (+1 +0)
                   (-1 +1) (+0 +1) (+1 +1))
                 (neighbors-of '(0 0))))
  (5am:is
   (equal
    '((-1 -1 -1) (+0 -1 -1) (+1 -1 -1) (-1 +0 -1) (+0 +0 -1) (+1 +0 -1) (-1 +1 -1) (+0 +1 -1) (+1 +1 -1)
      (-1 -1 +0) (+0 -1 +0) (+1 -1 +0) (-1 +0 +0)            (+1 +0 +0) (-1 +1 +0) (+0 +1 +0) (+1 +1 +0)
      (-1 -1 +1) (+0 -1 +1) (+1 -1 +1) (-1 +0 +1) (+0 +0 +1) (+1 +0 +1) (-1 +1 +1) (+0 +1 +1) (+1 +1 +1))
    (neighbors-of '(0 0 0)))))

(defun apply-rules (world coord)
  (let ((activep (active-p world coord))
        (active-neighbors (remove-if-not (partial #'active-p world)
                                         (neighbors-of coord))))

    (cond ((and activep (<= 2 (length active-neighbors) 3))
           (list #'activate coord))
          ((and activep (or (< (length active-neighbors) 2)
                            (> 3 (length active-neighbors))))
           (list #'inactivate coord))
          ((and (not activep) (= (length active-neighbors) 3))
           (list #'activate coord))
          (t nil))))

(defun tick (world)
  (let* ((active-coords (active-coords world))
         (all-to-consider (remove-duplicates
                           (mapcan
                            #'(lambda (coord) (append (list coord) (neighbors-of coord)))
                            active-coords)
                           :test #'equal))
         (commands (remove nil (mapcar (partial #'apply-rules world) all-to-consider))))
    (flet ((apply-command (w command) (funcall (first command) w (second command))))
      (reduce #'apply-command commands :initial-value world))))

(defun part1 (input num-generations)
  (let ((world (reduce #'(lambda (world coord) (activate world coord) world)
                       input :initial-value (make-world))))
    (dotimes (n num-generations world)
      (setf world (tick world)))))
