;;;
;;; examples:
;;; wire 1: ("R8" "U5" "L5" "D3")
;;; wire 2: ("U7" "R6" "D4" "L4")
;;; distance: 3+3=6
;;;
;;; wire 1: ("R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72")
;;; wire 2: ("U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83")
;;; distance: 159
;;;
;;; wire 1: ("R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51")
;;; wire 2: ("U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7")
;;; distance: 135
;;;

(load "../file-utils")
(load "../string-utils")

(defpackage :crossed-wires
  (:use :common-lisp)
  (:shadow :step))

(in-package :crossed-wires)

(defun parse-lines (lines) (mapcar #'(lambda (s) (string-utils:split s #\,)) lines))

(defparameter *raw-input* (parse-lines (file-utils:read-lines "./input.txt")))

(defparameter *origin* '(0 . 0))

(defparameter *test-cases*
  (list
    ;; distance 3
   (parse-lines '("R8,U5,L5,D3" "U7,R6,D4,L4"))
   ;; distance 159
   (parse-lines '("R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"))
    ;; distance 135
   (parse-lines '("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
   ))

(defun parse (instruction)
  (let ((direction (char instruction 0))
        (steps (parse-integer (subseq instruction 1))))
    (cons direction steps)))

(defun step (direction coord)
  (destructuring-bind (x . y) coord
    (case direction
      (#\R (cons (1+ x) y))
      (#\L (cons (1- x) y))
      (#\U (cons x (1+ y)))
      (#\D (cons x (1- y))))))

(defun perform (instruction current)
  (destructuring-bind (direction . steps) instruction
    (loop for n from 0 below steps
          collect (setf current (step direction current)))))

(defun make-path (instructions)
  (reduce #'(lambda (path instruction)
              (append path (perform instruction (car (last path)))))
          (mapcar #'parse instructions)
          :initial-value (list *origin*)))

(defun make-path-with-no-self-crosses (instructions)
  (remove-duplicates
   (make-path instructions)
   :test #'equal))

(defun count-path-refs (counts path)
  (reduce #'(lambda (counts coord)
            (incf (gethash coord counts 0))
            counts)
          path
          :initial-value counts))

(defun find-crosses (paths)
  (let ((all-refs
          (reduce #'count-path-refs paths
                          :initial-value (make-hash-table :test #'equal)))
        (crosses (list)))
    (maphash #'(lambda (k v) (when (> v 1) (push k crosses))) all-refs)
    (remove *origin* crosses :test #'equal)))

(defun manhattan-distance (coord paths)
  (declare (ignore paths))
  (+ (abs (- (car *origin*) (car coord)))
     (abs (- (cdr *origin*) (cdr coord)))))

(defun signal-delay (coord paths)
  (apply #'+ (mapcar #'(lambda (path) (position coord path :test #'equal)) paths)))

(defun nearest-cross (crosses calc-fn)
  (first (sort crosses #'< :key calc-fn)))

(defun find-nearest-cross (wires distance-fn)
  (let* ((paths (mapcar #'make-path wires))
         (no-self-cross (mapcar #'(lambda (path) (remove-duplicates path :test #'equal)) paths))
         (crosses (find-crosses no-self-cross)))
    (flet ((calc-fn (coord) (funcall distance-fn coord paths)))
      (calc-fn (nearest-cross crosses (function calc-fn))))))

#|
(find-nearest-cross *raw-input* #'manhattan-distance) ;; => 227
|#
