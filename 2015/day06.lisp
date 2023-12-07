(defpackage #:aoc-2015-06
  (:use :cl))

(in-package #:aoc-2015-06)

(aoc:def-today-suite*)

(defun parse-instruction (str)
  (let ((instr (aoc:split-string-on-char #\Space (string-upcase str))))
    (destructuring-bind (command from through to)
        (if (string= "TURN" (car instr)) (cdr instr) instr)
      (declare (ignore through))
      (list (aoc:keywordize command)
            (aoc:string-of-numbers->list-of-numbers from #\,)
            (aoc:string-of-numbers->list-of-numbers to #\,)))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-instruction))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun for-each-light (from to fn)
  (loop for x from (first from) to (first to)
        do (loop for y from (second from) to (second to)
                 do (funcall fn x y))))

(defclass light-grid ()
  ((grid :reader grid :initform (make-hash-table :test 'equalp))))

(defgeneric on (lights x y))
(defgeneric off (lights x y))
(defgeneric toggle (lights x y))

(defgeneric execute (lights command from to))
(defmethod execute (lights (command (eql :on)) from to)
  (for-each-light from to #'(lambda (x y) (on lights x y)))
  lights)
(defmethod execute (lights (command (eql :off)) from to)
  (for-each-light from to #'(lambda (x y) (off lights x y)))
  lights)
(defmethod execute (lights (command (eql :toggle)) from to)
  (for-each-light from to #'(lambda (x y) (toggle lights x y)))
  lights)
(defmethod execute (lights command from to)
  (format *error-output* "Unknown command: ~S (~S, ~S)" command from to)
  lights)

(defgeneric count-lights (lights))

(defclass on-off-light-grid (light-grid) ())

(defmethod on ((lights on-off-light-grid) x y)
  (setf (gethash (list x y) (grid lights)) t))
(defmethod off ((lights on-off-light-grid) x y)
  (setf (gethash (list x y) (grid lights)) nil))
(defmethod toggle ((lights on-off-light-grid) x y)
  (let ((current (gethash (list x y) (grid lights))))
    (setf (gethash (list x y) (grid lights)) (not current))))
(defmethod count-lights ((lights on-off-light-grid))
  (let ((count 0))
    (maphash #'(lambda (k v) (declare (ignore k)) (when v (incf count))) (grid lights))
    count))

(defun apply-instructions (lights instructions)
  (reduce #'(lambda (lights instr) (apply #'execute lights instr))
          instructions
          :initial-value lights))

(defun part1 (instructions)
  (count-lights (apply-instructions (make-instance 'on-off-light-grid) instructions)))

(5am:def-test part1 (:suite :aoc-2015-06)
  (5am:is (= 377891 (part1 +input+))))


(defclass brightness-light-grid (light-grid) ())

(defmethod on ((lights brightness-light-grid) x y)
  (incf (gethash (list x y) (grid lights) 0)))
(defmethod off ((lights brightness-light-grid) x y)
  (let ((current (gethash (list x y) (grid lights) 0)))
    (setf (gethash (list x y) (grid lights) 0)
          (if (zerop current) 0 (1- current)))))
(defmethod toggle ((lights brightness-light-grid) x y)
  (incf (gethash (list x y) (grid lights) 0) 2))
(defmethod count-lights ((lights brightness-light-grid))
  (let ((count 0))
    (maphash #'(lambda (k v) (declare (ignore k)) (incf count v)) (grid lights))
    count))

(defun part2 (instructions)
  (count-lights (apply-instructions (make-instance 'brightness-light-grid) instructions)))

(5am:def-test part2 (:suite :aoc-2015-06)
  (5am:is (= 14110788 (part2 +input+))))
