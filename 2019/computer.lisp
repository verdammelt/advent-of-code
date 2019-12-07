(defpackage #:computer
  (:use :common-lisp)
  (:export :compute :peek
           :run-program
           :computer
           :*debug-mode*
           :get-output))

(in-package #:computer)

(defparameter *debug-mode* nil)

(defclass computer ()
  ((instruction-pointer :initform 0)
   (memory :initform nil :initarg :memory)
   (state :initform :off :type (member :off :on :halt) :reader state)
   (input-stream :initform *standard-input* :initarg :input-stream)
   (output-stream :initform *standard-output* :initarg :output-stream)))

(defmethod print-object ((object computer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (state instruction-pointer) object
      (format stream "(STATE:~A PC:~A)" state instruction-pointer))))

;; instead of copy-seq here - create initialize-instance method that does that.
(defun make-computer (memory input-stream output-stream)
  (make-instance 'computer
                 :memory (copy-seq memory)
                 :input-stream input-stream
                 :output-stream output-stream))

(defgeneric get-output (computer))
(defmethod get-output ((computer computer))
  (get-output-stream-string (slot-value computer 'output-stream)))

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
  (with-slots (instruction-pointer input-stream) computer
    (when (interactive-stream-p input-stream)
      (format *query-io* "~&(~D)> " instruction-pointer))
    (read input-stream)))

(defun write-output (computer value)
  (with-slots (output-stream) computer
    (write value :stream output-stream)
    (when (interactive-stream-p output-stream)
      (fresh-line output-stream))))

; (fmakunbound 'execute)
(defgeneric execute (instruction modes computer))

(defmethod execute :around (instruction modes computer)
  (when *debug-mode*
    (format t "~&EXECUTE(~S ~S ~S)" instruction modes computer))
  (let ((result (call-next-method)))
    (when *debug-mode*
      (format t " => ~W~%" result))
    result))

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

(defmethod execute ((instruction (eql :jump-if-true)) modes (computer computer))
  (let ((args (get-parameters computer 2 modes)))
    (with-slots (instruction-pointer state) computer
      (setf state :running)
      (unless (zerop (first args))
        (setf instruction-pointer (second args))))))

(defmethod execute ((instruction (eql :jump-if-false)) modes (computer computer))
  (let ((args (get-parameters computer 2 modes)))
    (with-slots (instruction-pointer state) computer
      (setf state :running)
      (when (zerop (first args))
        (setf instruction-pointer (second args))))))

(defmethod execute ((instrution (eql :less-than)) modes (computer computer))
  (let ((args (get-parameters computer 2 modes))
        (result-location (get-immediate-parameter computer)))
    (with-slots (memory state) computer
      (setf state :running
            (address memory result-location) (if (apply #'< args) 1 0)))))

(defmethod execute ((instruction (eql :eql)) modes (computer computer))
  (let ((args (get-parameters computer 2 modes))
        (result-location (get-immediate-parameter computer)))
    (with-slots (memory state) computer
        (setf state :running
              (address memory result-location) (if (apply #'= args) 1 0)))))

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
                (5 :jump-if-true)
                (6 :jump-if-false)
                (7 :less-than)
                (8 :eql)
                (99 :halt))
              modes))))

(defun process-instruction (computer)
  (multiple-value-bind (op modes) (get-instruction computer)
   (funcall #'execute op modes computer)))

(defgeneric run-program (computer &optional additional-input))
(defmethod run-program ((computer computer) &optional additional-input)
  (when additional-input
    (with-slots (input-stream) computer
      (setf input-stream
            (make-concatenated-stream input-stream
                                      ;; because we will be READing from the and
                                      ;; we want the inputs to be distinct.
                                      (make-string-input-stream " ")
                                      additional-input))))
  (loop
     :until (eq (state computer) :halt)
     :do (process-instruction computer)
     :finally (return computer)))

(defun compute (initial-memory &key
                                 (input-stream *standard-input*)
                                 (output-stream *standard-output*))
  (run-program
   (make-computer initial-memory input-stream output-stream)))
