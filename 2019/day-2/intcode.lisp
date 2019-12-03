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

(defun modify-memory (memory updates)
  (dolist (update updates memory)
    (setf (address memory (first update)) (second update))))

;; correct answer: 4945026
(defun 1202-error (input-file)
  (peek
   (compute
    (modify-memory (read-input input-file)
                   '((1 12) (2 2))))
   0))

;; target from exercise = 19690720
;; solution: (52 96)
(defun what-noun-verb-causes (raw-input target)
  (let ((possibilities
          (loop for noun from 0 upto 99
                append (loop for verb from 0 upto 99
                             collect (list (list 1 noun) (list 2 verb))))))
    (loop for possibility in possibilities
          for memory = (modify-memory (copy-seq raw-input) possibility)
          for computer = (compute memory)
          ;; do (format t "Testing ~A ~&" possibility)
          when (= target (peek computer 0))
            do (return (mapcar #'second possibility)))))
