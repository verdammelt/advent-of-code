(defpackage #:aoc-2022-18
  (:use :cl))

(in-package #:aoc-2022-18)

(aoc:def-today-suite*)

(defun make-cube (x y z)
  (list x y z))

(defun cube-face-neigbors (cube)
  (let ((deltas '(
                  (1 0 0) (-1  0  0)
                  (0 1 0) ( 0 -1  0)
                  (0 0 1) ( 0  0 -1)
                  )))
    (flet ((apply-delta (delta) (mapcar #'+ cube delta)))
      (mapcar #'apply-delta (remove '(0 0 0) deltas :test #'equal)))))

(defun list->hash (cubes)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (c cubes) (setf (gethash c hash) t))
    hash))

(defun count-touching-cubes (cubes)
  (let ((all-neighbors (remove-duplicates (apply #'append (mapcar #'cube-neigbors cubes))
                                          :test #'equal))
        (world (list->hash cubes)))
    (count-if #'(lambda (n) (gethash n world)) all-neighbors)))

(defun parse-cube (str)
  (apply #'make-cube (mapcar #'parse-integer (aoc:split-string-on-char #\, str))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-cube))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun surface-area (cubes)
  (- (* 6 (length cubes))
     (count-touching-cubes cubes)
     1))

(5am:test surface-area
  (5am:is (= 6 (surface-area (list (make-cube 1 1 1)))))
  (5am:is (= 12 (surface-area (list (make-cube 1 1 1) (make-cube 100 100 100)))))
  (5am:is (= 10 (surface-area (list (make-cube 1 1 1) (make-cube 2 1 1)))))
  (5am:is (= 64 (surface-area +example+))))

(defun part1 (input) (surface-area input))

(5am:def-test part1 (:suite :aoc-2022-18)
  (5am:skip ":aoc-2022-18.1 not implemented")
  ;; (5am:is (= -1 (part1 +example+)))
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2022-18)
  (5am:skip ":aoc-2022-18.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
