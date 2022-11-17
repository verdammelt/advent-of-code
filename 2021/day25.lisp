(defpackage #:aoc-2021-25
  (:use :cl))

(in-package #:aoc-2021-25)

(aoc:def-today-suite*)

(defclass world ()
  ((dimensions :initarg :dimensions :reader dimensions)
   (south-herd :initarg :south-herd :accessor south-herd)
   (east-herd :initarg :east-herd :accessor east-herd)))

(defun east-of (world xy)
  (let ((dimensions (dimensions world)))
    (list (mod (1+ (first xy)) (first dimensions))
          (second xy))))

(defun south-of (world xy)
  (let ((dimensions (dimensions world)))
    (list (first xy)
          (mod (1+ (second xy)) (second dimensions)))))

(defun make-herd () (make-hash-table :test #'equal))

(defun parse-herds (data)
  (let ((data-array (aoc:lists->2d-array data))
        (south-herd (make-herd))
        (east-herd (make-herd)))
    (loop for y from 0 below (array-dimension data-array 0)
          do (loop for x from 0 below (array-dimension data-array 1)
                   do (ecase (aref data-array y x)
                        (#\v (setf (gethash (list x y) south-herd) t))
                        (#\> (setf (gethash (list x y) east-herd) t))
                        (#\. nil))))

    (make-instance 'world
                   :dimensions (reverse (array-dimensions data-array))
                   :east-herd east-herd
                   :south-herd south-herd)))

(defun read-data (file)
  (aoc:read-data file :post-process #'parse-herds))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun step-world (world)
  (let ((new-east (make-herd))
        (new-south (make-herd))
        (changed-p nil))

    (flet ((clear-p (xy &rest herds) (notany #'(lambda (h) (gethash xy h)) herds)))
      (alexandria:maphash-keys
       #'(lambda (xy)
           (let ((east (east-of world xy)))
             (if (clear-p east (east-herd world) (south-herd world))
                 (setf (gethash east new-east) t
                       changed-p t)
                 (setf (gethash xy new-east) t))))
       (east-herd world))
      (alexandria:maphash-keys
       #'(lambda (xy)
           (let ((south (south-of world xy)))
             (if (clear-p south new-east (south-herd world))
                 (setf (gethash south new-south) t
                       changed t)
                 (setf (gethash xy new-south) t))))
       (south-herd world)))

    (values (make-instance 'world :dimensions (dimensions world)
                                  :east-herd new-east
                                  :south-herd new-south)
            changed-p)))

(defun render-world (world &optional (stream *standard-output*))
  (fresh-line stream)
  (loop for y from 0 below (second (dimensions world))
        do (loop for x from 0 below (first (dimensions world))
                 do (format stream
                            "~C"
                            (cond ((gethash (list x y) (east-herd world)) #\>)
                                  ((gethash (list x y) (south-herd world)) #\v)
                                  (t #\.))))
        do (fresh-line stream)))

(defun step-until-no-change (world &optional (safety-limit most-positive-fixnum))
  (labels ((do-step (world i)
             (multiple-value-bind (new-world changed-p) (step-world world)
               (let ((new-i (1+ i)))
                 (cond ((not changed-p) (list new-i world :no-change))
                       ((< safety-limit new-i) (list new-i world :stoppped))
                       (t (do-step new-world new-i)))))))
    (do-step world 0)))

(defun part1 (input)
  (destructuring-bind (step world result) (step-until-no-change input)
      (declare (ignore world))
      (if (eq result :stoppped) nil step)))

(5am:def-test part1 (:suite :aoc-2021-25)
  (5am:is (= 58 (part1 +example+)))
  (5am:is (= 412 (part1 +input+))))

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2021-25)
  (5am:skip ":aoc-2021-25.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
