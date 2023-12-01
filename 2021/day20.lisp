(defpackage #:aoc-2021-20
  (:use :cl))

(in-package #:aoc-2021-20)

(aoc:def-today-suite*)

(defun parse-enhance-and-image (data)
  (destructuring-bind (enhance image) (aoc:split-lines-on-empty-line data)
    (pairlis '(:enhance :image)
             (list (apply #'concatenate 'string enhance)
                   (aoc:lists->2d-array image)))))

(defun read-data (file)
  (aoc:read-data file
                 :post-process #'parse-enhance-and-image))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

;; TODO: promote but with better default handling (may want an error sometimes...)
(defun get-cell (array x y default)
  (destructuring-bind (y-max x-max) (array-dimensions array)
    (cond ((or (minusp x) (minusp y)) default)
          ((or (>= x x-max) (>= y y-max)) default)
          (t (aref array y x)))))

(defun 9-neighbors (image x y default)
  (mapcar #'(lambda (xy) (funcall #'get-cell image (first xy) (second xy) default))
          `((,(1- x) ,(1- y)) (,x ,(1- y)) (,(1+ x) ,(1- y))
            (,(1- x) ,y)      (,x ,y)      (,(1+ x) ,y)
            (,(1- x) ,(1+ y)) (,x ,(1+ y)) (,(1+ x) ,(1+ y)))))

(defun parse-light-and-dark (c)
  (ecase c
    (#\. 0)
    (#\# 1)))

(defun neighbors-to-number (neighbors)
  (parse-integer (format nil "~{~B~}" (mapcar #'parse-light-and-dark neighbors))
                 :radix 2))

(defun enhance-lookup (enhance num) (char enhance num))

(defvar enlarge-factor 1)

(defun enlarge-image (image default)
  (let* ((new-array (make-array (mapcar #'(lambda (x) (+ x (* 2 enlarge-factor))) (array-dimensions image)))))
    (loop for y from (- enlarge-factor) below (+ enlarge-factor (array-dimension image 0))
          do (loop for x from (- enlarge-factor) below (+ enlarge-factor (array-dimension image 1))
                   do (setf (aref new-array (+ enlarge-factor y) (+ enlarge-factor x))
                            (get-cell image x y default))))
    new-array))

(defun enhance-image (image enhance default)
  (flet ((lookup (array x y)
           (let ((enhance-idx (neighbors-to-number (9-neighbors array x y default))))
             (enhance-lookup enhance enhance-idx))))
    (aoc:map-2d-array #'(lambda (array row col) (lookup array col row)) (enlarge-image image default))))

(defun 2darray->strings (array)
  (loop for y from 0 below (array-dimension array 0)
        collect (concatenate 'string
                        (loop for x from 0 below (array-dimension array 1)
                              collect (aref array y x)))))

(defun display-image (image-array &optional (stream t))
  (format stream "~{~A~%~}" (2darray->strings (enlarge-image image-array #\-))))

(defun count-lit (image)
  (loop for y from 0 below (array-dimension image 0)
        sum (loop for x from 0 below (array-dimension image 1)
                  sum (parse-light-and-dark (get-cell image x y #\.)))))

(defun all-neighbors-the-same (c) (loop for i below 9 collect c))

(defun do-n-enhance (n image enhance)
  (loop for i below n
        for default = #\.
          then (enhance-lookup enhance (neighbors-to-number (all-neighbors-the-same default)))
        for new-image = (enhance-image image enhance default)
          then (enhance-image new-image enhance default)
        finally (return new-image)))

(defun part1 (input)
  "Enhance twice and count lit #\# cells"
  (count-lit (do-n-enhance 2 (cdr (assoc :image input)) (cdr (assoc :enhance input)))))

(5am:def-test part1 (:suite :aoc-2021-20)
  (5am:is (= 35 (part1 +example+)))
  (5am:is (= 5461 (part1 +input+))))

(defun part2 (input)
  "Enhance 50 times and count lit cells"
  (count-lit (do-n-enhance 50 (cdr (assoc :image input)) (cdr (assoc :enhance input)))))

(5am:def-test part2 (:suite :aoc-2021-20)
  (5am:is (= 3351 (part2 +example+)))
  (5am:is (= 18226 (part2 +input+))))
