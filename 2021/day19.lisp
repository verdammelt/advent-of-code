(defpackage #:aoc-2021-19
  (:use :cl))

(in-package #:aoc-2021-19)

(aoc:def-today-suite*)

(defun careful-parse-coordinate (l)
  (if (or (aoc:empty-string-p l)
          (string= "---" l :end2 3))
      ""
      (aoc:string-of-numbers->list-of-numbers l :delimiters #\,)))

(defun make-coord (x y z) (list x y z))
(defun offset-coord (coord offset)
  (apply #'make-coord (mapcar #'+ coord offset)))
(defun rotate-coord (coord rotation)
  (funcall rotation coord))

(defun make-scanner (beacon-data)
  (pairlis '(:position :beacons)
            (list (make-coord 0 0 0)
                  (mapcar #'(lambda (xyz) (apply #'make-coord xyz)) beacon-data))))

(defun make-scanners (data)
  (mapcar #'make-scanner (aoc:split-lines-on-empty-line data)))

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'careful-parse-coordinate
                 :post-process #'make-scanners))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

;; all transforms
;;
#|
( x  y  z)
((- y)  x  z)
((- x) (- y)  z)
( y (- x)  z)
( x  y (- z))
((- y)  x (- z))
((- x) (- y) (- z))
( y (- x) (- z))
( z  y (- x))
((- y)  z (- x))
((- z) (- y) (- x))
( y (- z) (- x))
( z  y  x)
((- y)  z  x)
((- z) (- y)  x)
( y (- z)  x)
( x  z (- y))
((- z)  x (- y))
((- z) (- x) (- y))
( x (- z) (- y))
( x  z  y)
((- z)  x  y)
((- z) (- x)  y)
( x (- z)  y)
|#

(defun part1 (input) (declare (ignore input)) 0)

;; TODO: complete 2021-19.1
(5am:def-test part1 (:suite :aoc-2021-19)
  (5am:skip ":aoc-2021-19.1 not implemented")
  ;; (5am:is (= -1 (part1 +example+)))
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

;; TODO: complete 2021-19.2
(5am:def-test part2 (:suite :aoc-2021-19)
  (5am:skip ":aoc-2021-19.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
