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

(defun make-region (map plant)
  (list :map map :plant plant :plots (list)))

(defun region-map (region) (getf region :map))
(defun region-plant (region) (getf region :plant))
(defun region-plots (region) (getf region :plots))
(defun (setf region-plots) (new-value region) (setf (getf region :plots) new-value))
(defun region-perimeter (region)
  (let ((plots (region-plots region))
        (plant (region-plant region))
        (map (region-map region)))
    (count-if-not #'(lambda (p) (plant-equal plant p))
                  (mapcan
                   #'(lambda (p) (mapcar #'(lambda (n) (aoc:coord-safe-aref map n)) (coord-neighbors p)))
                   plots))))
(defun region-area (region) (length (region-plots region)))

(defun plot-in-region (plot region)
  (member plot (region-plots region) :test #'aoc:coord-equal))

(defun coord-neighbors (coord)
  (mapcar #'(lambda (dir) (aoc:coord-add coord dir))
          (list (aoc:make-coord -1 0)
                (aoc:make-coord 0 1)
                (aoc:make-coord 1 0)
                (aoc:make-coord 0 -1))))

(defun gather-regions (map)
  (do ((all-plots (all-coords-for-map map))
       (all-regions (list))
       (working-edge (list))
       (current-region nil)
       (next nil))
      ((null all-plots) (push current-region all-regions))

   (cond
      ((not current-region)
       (setf working-edge (list (car all-plots))
             current-region (make-region map (aoc:coord-safe-aref map (car working-edge)))))

      (next
       (when (plant-equal (aoc:coord-safe-aref map next) (region-plant current-region))
         (pushnew next (region-plots current-region) :test #'aoc:coord-equal)
         (setf all-plots (remove next all-plots :test #'aoc:coord-equal))
         (let ((neighbors (coord-neighbors next)))
           (setf working-edge (append working-edge
                                      (remove-if
                                       #'(lambda (p) (plot-in-region p current-region))
                                       neighbors)))))
       (setf next nil))

      (working-edge (setf next (car working-edge)
                          working-edge (cdr working-edge)))

      (t (push current-region all-regions)
         (setf current-region nil)))))

(defun part1 (input)
  (let ((regions (gather-regions input)))
    (aoc:sum (mapcar #'(lambda (r) (* (region-area r) (region-perimeter r))) regions))))

(5am:def-test part1 (:suite :aoc-2024-12)
  (5am:is (= 1930 (part1 +example+)))
  (5am:skip ":aoc-2024-12.1 does not work on real input")
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2024-12)
  (5am:skip ":aoc-2024-12.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
