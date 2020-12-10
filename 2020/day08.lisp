(defpackage #:aoc-2020-08
  (:use :cl :aoc :aoc-2020/utils))

(in-package #:aoc-2020-08)

(defun parse-op (line)
  (let ((op-and-offset (split-sequence:split-sequence #\Space line)))
    (cons (intern (string-upcase (first op-and-offset)) :keyword)
          (parse-integer (second op-and-offset)))))

(defun list->vector (ops)
  (coerce ops 'vector))

(defparameter +input+ (read-data (today-data)
                                 :line-parser #'parse-op
                                 :post-process #'list->vector))
(defparameter +example+ (read-data (today-data "example")
                                   :line-parser #'parse-op
                                   :post-process #'list->vector))

(defun copy-tape (tape)
  (map 'vector #'copy-list tape))

(defun tape-equal (tape1 tape2) (equalp tape1 tape2))

(defun op-on-tape (ip tape)
  (elt tape ip))

(defun (setf op-on-tape) (new-value ip tape)
  (setf (elt tape ip) new-value))

(defclass machine ()
  ((tape :documentation "Program tape"
         :initarg :tape
         :reader machine-tape)
   (acc :documentation "Accumulator"
        :initform 0
        :accessor machine-acc)
   (ip :documentation "Instruction Pointer"
       :initform 0
       :accessor machine-ip)
   (ip-history :documentation "Log of all values of ip"
               :initform (list)
               :accessor machine-ip-history)))

(defun make-machine (tape)
  (make-instance 'machine :tape tape))

;; TODO: isn't this how i do this sort of required argument????
;; (defmethod initialize-instance :after ((machine machine) &rest initargs &key &allow-other-keys)
;;   (declare (ignore initargs))
;;   (unless (slot-boundp machine 'tape)
;;     (error "MACHINE must be given a TAPE when constructed")))

(defmethod print-object ((machine machine) stream)
  (print-unreadable-object (machine stream :type t)
    (format stream "~&ACC: ~D~&IP: ~D~&History: ~S"
            (machine-acc machine)
            (machine-ip machine)
            (machine-ip-history machine))))

(define-condition machine-condition (error)
  ((machine :initarg :machine :reader machine-condition-machine))
  (:report (lambda (condition stream)
             (format stream "A Condition occured on ~S"
                     (machine-condition-machine condition)))))

(define-condition machine-instruction-loop (machine-condition) ()
  (:report (lambda (condition stream)
             (format stream "Instruction Pointer has been seen before: ~S"
                     (machine-condition-machine condition)))))

(define-condition machine-instruction-out-of-bounds (machine-condition) ()
  (:report (lambda (condition stream)
             (format stream "Instruction Pointer out of bounds: ~S"
                     (machine-condition-machine condition)))))

(defgeneric op (machine op arg))

(defmethod op :before (machine op arg)
  (declare (ignore op arg))
  (when (member (machine-ip machine) (machine-ip-history machine) :test #'=)
    (error 'machine-instruction-loop :machine machine))
  (push (machine-ip machine) (machine-ip-history machine)))

(defmethod op :after (machine op arg)
  (declare (ignore op arg))
  (incf (machine-ip machine)))

(defmethod op (machine (op (eql :acc)) arg)
  (incf (machine-acc machine) arg))

(defmethod op (machine (op (eql :nop)) arg)
  (declare (ignore machine op arg)))

(defmethod op (machine (op (eql :jmp)) arg)
  (incf (machine-ip machine) (1- arg)))

(defun current-op (machine)
  (let ((ip (machine-ip machine))
        (tape (machine-tape machine)))
    (unless (< -1 ip (length tape))
      (error 'machine-instruction-out-of-bounds :machine machine))
    (op-on-tape ip tape)))

(defun execute-current-op (machine)
  (destructuring-bind (op . arg)
      (current-op machine)
    (op machine op arg)
    machine))

(defun run (machine)
  (run (execute-current-op machine)))

(defun run-until-loop (machine)
  (handler-case (progn (run machine)
                       (error "Expected and Earth-shattering Kaboom!"))
    (machine-instruction-loop (condition) (machine-condition-machine condition))))

(assert (= 5 (machine-acc (run-until-loop (make-machine +example+)))))
(assert (= 1451 (machine-acc (run-until-loop (make-machine +input+)))))

(defun instructions-executed (machine)
  (let ((tape (machine-tape machine))
        (ip-history (machine-ip-history machine)))
    (mapcar #'(lambda (ip) (list ip (op-on-tape ip tape))) (reverse ip-history))))

(defparameter +op-mapping+ '((:acc . :acc) (:jmp . :nop) (:nop . :jmp)))

(defun make-alternate-tapes (tape instructions-to-modify
                             &optional (mapping +op-mapping+))
  (labels ((map-op (op-and-arg) (cdr (assoc (car op-and-arg) mapping)))
           (mod-tape (to-modify)
             (destructuring-bind (ip op) to-modify
               (let ((new-tape (copy-tape tape)))
                 (setf (car (op-on-tape ip new-tape)) (map-op op))
                 new-tape))))
    (remove-if  #'(lambda (new-tape) (tape-equal new-tape tape))
                (mapcar #'mod-tape instructions-to-modify))))

(defun try-to-fix (machine)
  (let ((machine (run-until-loop machine)))
    (handler-case (progn (mapcar #'(lambda (tape) (run-until-loop (make-machine tape)))
                                  (make-alternate-tapes (machine-tape machine)
                                                        (instructions-executed machine)))
                         (error "Now fix found for ~S" machine))
      (machine-instruction-out-of-bounds (condition) (machine-condition-machine condition)))))

(assert (= 8 (machine-acc (try-to-fix (make-machine +example+)))))
(assert (= 1160 (machine-acc (try-to-fix (make-machine +input+)))))
