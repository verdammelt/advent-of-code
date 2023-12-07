(defpackage #:aoc-2019-19
  (:use :cl))

(in-package #:aoc-2019-19)

(aoc:def-today-suite*)

(defun load-program (file)
  (aoc:string-of-numbers->list-of-numbers
   (car (aoc:read-data file)) #\,))

(defun check-xy (program x y)
  (with-output-to-string (output)
    (computer:compute
     program
     :input-stream (make-string-input-stream (format nil "~D ~D" x y))
     :output-stream output)))

(defun scan-area (program max-x max-y &optional (start-x 0) (start-y 0))
  (let* ((map (make-array (list max-y max-x)
                          :element-type 'character
                          :initial-element #\?)))
    (dotimes (x max-x)
      (dotimes (y max-y)
        (let* ((at-spot (check-xy program (+ x start-x ) (+ y start-y)))
               (map-value (if (= 1 (parse-integer at-spot)) #\# #\.)))
          (setf (aref map y x) map-value))))
    map))

(defun count-beam-pulling (map)
  (loop for idx from 0 below (array-total-size map)
     count (char= (row-major-aref map idx) #\#)))

(defun print-map (map &optional (output *standard-output*))
  (loop for x from 0 below (array-dimension map 1)
     do (loop for y from 0 below (array-dimension map 0)
           do (format output "~C" (aref map y x))
              finally (terpri output))))

(5am:def-test part1 (:suite :aoc-2019-19)
  (5am:is (= 213 (count-beam-pulling (scan-area (load-program (aoc:today-data-pathname)) 50 50)))))

;; -------------------- part ii --------------------

(defun beam-active-p (program x y)
  (unless (or (minusp x) (minusp y))
    (= 1 (parse-integer (check-xy program x y)))))

(defun square-fits-p (program x y size)
  "Check if square of size SIZE is entirely in tractor beam.
The square is defined as having a lower-left corner at x, y."
  (and (beam-active-p program x y)
       (beam-active-p program (+ x size) (- y size))))

(defun first-x-with-active-beam (program y &optional (start-x 0))
  (do ((x start-x (1+ x)))
      ((beam-active-p program x y) x)))

(defun part2 (file size &optional (step 1) start-y)
  (let ((program (load-program file)))
    (do* ((y (or start-y (1- size)) (+ y step))
          (x (first-x-with-active-beam program y)
             (first-x-with-active-beam program y x)))
         ((square-fits-p program x y (1- size))
          (+ (* 10000 x) (1+ (- y size)))))))

(5am:def-test part2 (:suite :aoc-2019-19)
  (5am:is (= 7830987 (part2 (aoc:today-data-pathname) 100))))
