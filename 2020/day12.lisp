(defpackage #:aoc-2020-12
  (:use :cl #:aoc))

(in-package #:aoc-2020-12)
(aoc:def-today-suite*)

(defun manhattan-distance (xy1 xy2)
  (let ((x1 (first xy1)) (y1 (second xy1))
        (x2 (first xy2)) (y2 (second xy2)))
    (+ (abs (- x1 x2))
       (abs (- y1 y2)))))

(defun make-navcomm (command value)
  (list command value))
(defun navcomm-command (navcomm) (first navcomm))
(defun navcomm-value (navcomm) (second navcomm))

(defun parse-navcomm (str)
  (let ((commands '((#\N . :north)
                    (#\S . :south)
                    (#\E . :east)
                    (#\W . :west)
                    (#\L . :left)
                    (#\R . :right)
                    (#\F . :forward))))
    (make-navcomm
     (cdr (assoc (char str 0) commands :test #'char=))
     (parse-integer (subseq str 1)))))

(defparameter +example+ (read-data (today-data-pathname "example")
                                   :line-parser #'parse-navcomm))
(defparameter +input+ (read-data (today-data-pathname)
                                 :line-parser #'parse-navcomm))

(defclass ferry ()
  ((position :initarg :position :reader ferry-position)
   (facing :initarg :facing :reader ferry-facing)
   (waypoint :initarg :waypoint :reader ferry-waypoint)))

(defun make-ferry (position facing &optional waypoint)
  (make-instance 'ferry :position position :facing facing :waypoint waypoint))

(defmethod print-object ((object ferry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream " FACING: ~S POSITION: ~S"
            (ferry-position object)
            (ferry-facing object))
    (when (ferry-waypoint object)
      (format stream " WAYPOINT: ~S" (ferry-waypoint object)))))

(defparameter +turns+ '((:north . :east)
                        (:east . :south)
                        (:south . :west)
                        (:west . :north)))

(defun turn-right (facing)
  (cdr (assoc facing +turns+)))
(defun turn-left (facing)
  (car (rassoc facing +turns+)))

(defgeneric do-command (command value ferry)
  (:method (command value ferry)
    (error "Cannot handle DO-COMMAND with args ~S" (list command value ferry))))

(defmethod do-command ((command (eql :forward)) value ferry)
  (do-command (ferry-facing ferry) value ferry))

(defmethod do-command ((command (eql :north)) value ferry)
  (let ((position (ferry-position ferry)))
    (make-ferry (list (first position) (+ (second position) value))
                (ferry-facing ferry))))

(defmethod do-command ((command (eql :east)) value ferry)
  (let ((position (ferry-position ferry)))
    (make-ferry (list (+ (first position) value) (second position))
                (ferry-facing ferry))))

(defmethod do-command ((command (eql :south)) value ferry)
  (do-command :north (* -1 value) ferry))

(defmethod do-command ((command (eql :west)) value ferry)
  (do-command :east (* -1 value) ferry))

(defmethod do-command ((command (eql :right)) (value (eql 90)) ferry)
  (make-ferry (ferry-position ferry) (turn-right (ferry-facing ferry))))
(defmethod do-command ((command (eql :right)) (value (eql 180)) ferry)
  (do-command :right 90 (do-command :right 90 ferry)))
(defmethod do-command ((command (eql :right)) (value (eql 270)) ferry)
  (do-command :left 90 ferry))

(defmethod do-command ((command (eql :left)) (value (eql 90)) ferry)
  (make-ferry (ferry-position ferry) (turn-left (ferry-facing ferry))))
(defmethod do-command ((command (eql :left)) (value (eql 180)) ferry)
  (make-ferry (ferry-position ferry)
              (turn-left (turn-left (ferry-facing ferry)))))
(defmethod do-command ((command (eql :left)) (value (eql 270)) ferry)
  (do-command :right 90 ferry))

(defun follow-instructions (navcomms ferry &optional (command-processor #'do-command))
  (reduce #'(lambda (ferry navcomm)
              (destructuring-bind (command value) navcomm
                  (funcall command-processor command value ferry)))
          navcomms
          :initial-value ferry))

(defun part1 (instructions)
  (let ((starting-position (list 0 0)))
    (manhattan-distance
     starting-position
     (ferry-position (follow-instructions instructions
                                          (make-ferry starting-position :east))))))

(5am:def-test part1 (:suite :aoc-2020-12)
  (5am:is (= 25 (part1 +example+)))
  (5am:is (= 1186 (part1 +input+))))

(defgeneric do-waypoint-command (command value ferry)
  (:method (command value ferry)
    (error "Cannot handle DO-WAYPOINT-COMMAND with args ~S" (list command value ferry))))

(defmethod do-waypoint-command ((command (eql :forward)) value ferry)
  (let ((position (ferry-position ferry))
        (waypoint (ferry-waypoint ferry)))
    (let ((new-position (list (+ (first position)
                                 (* value (first waypoint)))
                              (+ (second position)
                                 (* value (second waypoint))))))
      (make-ferry new-position (ferry-facing ferry) waypoint))))

(defmethod do-waypoint-command ((command (eql :north)) value ferry)
  (destructuring-bind (east north) (ferry-waypoint ferry)
    (make-ferry (ferry-position ferry) (ferry-facing ferry)
                (list east (+ north value)))))

(defmethod do-waypoint-command ((command (eql :south)) value ferry)
  (do-waypoint-command :north (* -1 value) ferry))

(defmethod do-waypoint-command ((command (eql :east)) value ferry)
  (destructuring-bind (east north) (ferry-waypoint ferry)
    (make-ferry (ferry-position ferry) (ferry-facing ferry)
                (list (+ east value) north))))
(defmethod do-waypoint-command ((command (eql :west)) value ferry)
  (do-waypoint-command :east (* -1 value) ferry))

(defmethod do-waypoint-command ((command (eql :right)) (value (eql 90)) ferry)
  (destructuring-bind (east north) (ferry-waypoint ferry)
    (make-ferry (ferry-position ferry) (ferry-facing ferry)
                (list north (* -1 east)))))
(defmethod do-waypoint-command ((command (eql :right)) (value (eql 180)) ferry)
  (do-waypoint-command :right 90 (do-waypoint-command :right 90 ferry)))
(defmethod do-waypoint-command ((command (eql :right)) (value (eql 270)) ferry)
  (do-waypoint-command :right 90
    (do-waypoint-command :right 90
      (do-waypoint-command :right 90 ferry))))

(defmethod do-waypoint-command ((command (eql :left)) (value (eql 90)) ferry)
  (do-waypoint-command :right 270 ferry))
(defmethod do-waypoint-command ((command (eql :left)) (value (eql 180)) ferry)
  (do-waypoint-command :right 180 ferry))
(defmethod do-waypoint-command ((command (eql :left)) (value (eql 270)) ferry)
  (do-waypoint-command :right 90 ferry))

(defun part2 (instructions)
  (let ((position (list 0 0))
        (waypoint (list 10 1)))
    (manhattan-distance
     position
     (ferry-position (follow-instructions instructions
                                          (make-ferry position :east waypoint)
                                          #'do-waypoint-command)))))

(5am:def-test part2 (:suite :aoc-2020-12)
  (5am:is (= 286 (part2 +example+)))
  (5am:is (= 47806 (part2 +input+))))
