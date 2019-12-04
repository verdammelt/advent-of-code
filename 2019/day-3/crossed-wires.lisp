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

(defpackage :crossed-wires
  (:use :common-lisp)
  (:shadow :step))

(in-package :crossed-wires)

;;; from day-1
;;; TODO: make common place for utilities
(defun file-lines (file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
          while line collect line)))

;;; from day-2
;;; TODO: make common place for utilities
(defun split-string (string &optional (delimeter #\Space))
  (labels ((split-string-iter (string result)
             (let ((idx (position delimeter string :from-end t)))
               (if idx
                   (split-string-iter (subseq string 0 idx)
                                      (cons (subseq string (1+ idx)) result))
                   (cons string result)))))
    (split-string-iter string (list))))

(defun parse-lines (lines) (mapcar #'(lambda (s) (split-string s #\,)) lines))

(defparameter *raw-input* (parse-lines (file-lines "./input.txt")))

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
    (mapcar #'(lambda (coord) (list (manhattan-distance coord) coord))
            (remove *origin* crosses :test #'equal))))

(defun manhattan-distance (coord)
  (+ (abs (- (car *origin*) (car coord)))
     (abs (- (cdr *origin*) (cdr coord)))))

(defun nearest-cross (crosses)
  (car (sort crosses #'< :key #'car)))

(defun find-nearest-cross-by-distance (wires)
  (let* ((paths (mapcar #'make-path-with-no-self-crosses wires))
         (crosses (find-crosses paths)))
    (nearest-cross crosses)))
