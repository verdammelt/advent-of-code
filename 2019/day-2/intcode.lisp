(defpackage :intcode
  (:use :common-lisp))

(in-package :intcode)

(defun split-string (string &optional (delimeter #\Space))
  (labels ((split-string-iter (string result)
             (let ((idx (position delimeter string :from-end t)))
               (if idx
                   (split-string-iter (subseq string 0 idx)
                                      (cons (subseq string (1+ idx)) result))
                   (cons string result)))))
    (split-string-iter string (list))))

(defun read-input (file)
  (with-open-file (stream file :direction :input)
    (mapcar #'parse-integer (split-string (read-line stream) #\,))))

(defun get-register (tape idx)
  (if (< (length tape) idx) nil
      (elt tape idx)))
(defun (setf get-register) (new-value tape idx)
  (setf (elt tape idx) new-value))

(defun write-to-tape (tape idx value)
  (setf (get-register tape idx) value)
  tape)

(defun compute (tape pc)
  (labels ((add (arg1 arg2 result-location pc)
             (compute (write-to-tape tape result-location (+ arg1 arg2)) pc))
           (mul (arg1 arg2 result-location pc)
             (compute (write-to-tape tape result-location (* arg1 arg2)) pc))
           (halt (arg1 arg2 result-location pc)
             (declare (ignore arg1 arg2 result-location pc))
             tape)
           (lookup-opcode (op)
             (ecase op
               (1 #'add)
               (2 #'mul)
               (99 #'halt))))
    (if (< (length tape) pc) tape
        (progn (let ((op (lookup-opcode (get-register tape pc)))
                     (arg1-location (get-register tape (+ pc 1)))
                     (arg2-location (get-register tape (+ pc 2)))
                     (result-location (get-register tape (+ pc 3))))
                 (funcall op
                          (get-register tape arg1-location)
                          (get-register tape arg2-location)
                          result-location
                          (+ pc 4)))))))

(defun execute (input-file)
  (let ((tape (read-input input-file))
        (pc 0))
    (compute tape pc)))

;; correct answer: 4945026
(defun 1202-error (input-file)
  (let ((raw-tape (read-input input-file))
        (pc 0))
    (let* ((tape (write-to-tape raw-tape 1 12))
           (tape (write-to-tape tape 2 2)))
      (compute tape pc))))
