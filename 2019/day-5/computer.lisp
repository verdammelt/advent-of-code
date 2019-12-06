(defpackage #:computer
  (:use :common-lisp)
  (:export :compute))

(in-package #:computer)

(defclass computer ()
  ((instruction-pointer :initform 0)
   (memory :initform nil :initarg :memory)
   (state :initform :off :type (member :off :on :halt) :reader state)
   (input-stream :initform *standard-input* :initarg :input-stream)
   (output-stream :initform *standard-output* :initarg :output-stream)))

(defun make-computer (memory input-stream output-stream)
  (make-instance 'computer
                 :memory memory
                 :input-stream input-stream
                 :output-stream output-stream))

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

(defun immediate-address-p (idx modes)
  (when (< idx (length modes))
    (= (nth idx modes) 1)))

(defun get-parameters (computer num-parameters modes)
  (with-slots (instruction-pointer memory) computer
    (loop
       :for idx :from 0 :below num-parameters
       :for value := (get-next-memory computer)
       :if (immediate-address-p idx modes) :collect value
       :else :collect (address memory value))))

(defun get-immediate-parameter (computer)
  (get-next-memory computer))

(defun read-input (computer)
  (format *query-io* "~&(~D)> " (slot-value computer 'instruction-pointer))
  (read (slot-value computer 'input-stream)))

(defun write-output (computer value)
  (with-slots (output-stream) computer
    (write value :stream output-stream)
    (fresh-line output-stream)))

; (fmakunbound 'execute)
(defgeneric execute (instruction modes computer))
(defmethod execute ((instruction (eql :add)) modes (computer computer))
  (let ((args (get-parameters computer 2 modes))
        (result-location (get-immediate-parameter computer)))
    (with-slots (memory state) computer
      (setf state :running
            (address memory result-location) (apply #'+ args)))))

(defmethod execute ((instruction (eql :mul)) modes (computer computer))
  (let ((args (get-parameters computer 2 modes))
        (result-location (get-immediate-parameter computer)))
    (with-slots (memory state) computer
      (setf state :running
            (address memory result-location) (apply #'* args)))))


(defmethod execute ((instruction (eql :inp)) modes (computer computer))
  (let ((result-location (get-immediate-parameter computer)))
    (with-slots (memory state) computer
      (setf state :running
            (address memory result-location)
            (read-input computer)))))

(defmethod execute ((instruction (eql :out)) modes (computer computer))
  (let ((args (get-parameters computer 1 modes)))
    (with-slots (state) computer
      (write-output computer (car args))
      (setf state :running))))

(defmethod execute ((instruction (eql :halt)) modes (computer computer))
  (setf (slot-value computer 'state) :halt))

(defun parse-instruction (raw-instruction)
  (multiple-value-bind (modes opcode) (floor raw-instruction 100)
    (let ((modes (loop for c across (reverse (format nil "~3,'0d" modes))
                      collect (digit-char-p c))))
      (values opcode modes))))

(defun get-instruction (computer)
  (let ((raw-instruction (get-next-memory computer)))
    (multiple-value-bind (op-code modes)
        (parse-instruction raw-instruction)
      (values (ecase op-code
                (1 :add)
                (2 :mul)
                (3 :inp)
                (4 :out)
                (99 :halt))
              modes))))

(defun process-instruction (computer)
  (multiple-value-bind (op modes) (get-instruction computer)
   (funcall #'execute op modes computer)))

(defun compute (initial-memory &key
                                 (input-stream *standard-input*)
                                 (output-stream *standard-output*))
  (loop
    with computer = (make-computer initial-memory input-stream output-stream)
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
;; immediate mode (first parameter)
(assert (= (peek (compute '(101 2 6 0 99 X 2)) 0) 4))
;; immediate mode (second parameter)
(assert (= (peek (compute '(1001 5 2 0 99 2 X)) 0) 4))
;; immediate mode (all parameters)
(assert (= (peek (compute '(1101 2 2 0 99)) 0) 4))
;; example program (will terminate because 99 written to next PC
(assert (eq (slot-value (compute '(1002 4 3 4 33)) 'state) :halt))
;; input
(with-input-from-string (input-stream "666")
  (assert (= (peek (compute '(3 0 99)
                            :input-stream input-stream)
                   0)
             666)))
;; output
(assert (string= (with-output-to-string (output-stream)
                   (compute '(1101 2 3 0 4 0 99)
                            :output-stream output-stream))
                 (format nil "5~&")))
;; example program (input/output)
(let ((expected "13"))
  (assert (string= (with-output-to-string (output-stream)
                     (with-input-from-string (input-stream expected)
                       (compute '(3 0 4 0 999)
                                :input-stream input-stream
                                :output-stream output-stream)))
                   (format nil "~A~&" expected))))
