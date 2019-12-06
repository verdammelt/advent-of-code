(defpackage #:computer
  (:use :common-lisp)
  (:export :compute))

(in-package #:computer)

(defclass computer ()
  ((instruction-pointer :initform 0)
   (memory :initform nil :initarg :memory)
   (state :initform :off :type (member :off :on :halt) :reader state)))

(defun make-computer (memory)
  (make-instance 'computer :memory memory))

(defun address (memory idx)
  (elt memory idx))
(defun (setf address) (new-value memory idx)
  (setf (elt memory idx) new-value))

(defun get-next-memory (computer)
  (with-slots (instruction-pointer memory) computer
    (prog1 (address memory instruction-pointer)
      (incf instruction-pointer))))

(defun peek (computer pointer)
  (address (slot-value computer 'memory) pointer))

(defun get-parameters (computer num-parameters)
  (with-slots (instruction-pointer memory) computer
    (loop for idx from 1 to num-parameters
            collect (get-next-memory computer))))

(defgeneric execute (instruction computer))
(defmethod execute ((instruction (eql :add)) (computer computer))
  (destructuring-bind (arg1-location arg2-location result-location)
      (get-parameters computer 3)
    (with-slots (memory state) computer
      (let ((arg1 (address memory arg1-location))
            (arg2 (address memory arg2-location)))
        (setf state :running
              (address memory result-location) (+ arg1 arg2))))))

(defmethod execute ((instruction (eql :mul)) (computer computer))
  (destructuring-bind (arg1-location arg2-location result-location)
      (get-parameters computer 3)
    (with-slots (memory state) computer
      (let ((arg1 (address memory arg1-location))
            (arg2 (address memory arg2-location)))
        (setf state :running
              (address memory result-location) (* arg1 arg2))))))

(defmethod execute ((instruction (eql :halt)) (computer computer))
  (setf (slot-value computer 'state) :halt))

(defun get-instruction (computer)
  (ecase (get-next-memory computer)
    (1 :add)
    (2 :mul)
    (99 :halt)))

(defun process-instruction (computer)
  (funcall #'execute (get-instruction computer) computer))

(defun compute (initial-memory)
  (loop
    with computer = (make-computer initial-memory)
    until (eq (state computer) :halt)
    do (process-instruction computer)
     finally (return computer)))

;;;
;;; tests
;;;
;; add
(assert (= (peek (compute '(1 5 6 0 99 2 2)) 0) 4))
;; mul
(assert (= (peek (compute '(2 5 6 0 99 2 3)) 0) 6))
;; immediate mode (first parameter
;(assert (= (peek (compute '(101 2 6 0 99 X 2)) 0) 4))
