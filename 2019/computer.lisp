; (delete-package (find-package :computer))
(defpackage #:computer
  (:use :common-lisp)
  (:export :compute :peek :poke
           :make-computer
           :run-program
           :computer
           :*debug-mode*
           :get-state
           :get-output
           :halted-p
           :load-program))

(in-package #:computer)

(defparameter *debug-mode* nil)

(defclass computer ()
  ((instruction-pointer :initform 0)
   (reference-base :initform 0)
   (memory :initform nil :initarg :memory)
   (state :initform :off :type (member :off :on :halt :blocked-input)
          :reader get-state)
   (input-stream :initform *standard-input* :initarg :input-stream)
   (output-stream :initform *standard-output*
                  :initarg :output-stream)))

(defmethod initialize-instance :after ((obj computer) &key &allow-other-keys)
  (with-slots (memory) obj
    (setf memory (make-array (length memory) :initial-contents memory))))

(defgeneric get-output (computer))
(defmethod get-output ((computer computer))
  (get-output-stream-string (slot-value computer 'output-stream)))

(defgeneric halted-p (computer))
(defmethod halted-p ((computer computer))
  (eq (get-state computer) :halt))

(defmethod print-object ((object computer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (state instruction-pointer reference-base memory) object
      (format stream "(STATE:~A PC:~A REF:~A MEMSIZE:~A)"
              state
              instruction-pointer reference-base
              (length memory)))))

(defun make-computer (memory input-stream output-stream)
  (make-instance 'computer
                 :memory memory
                 :input-stream input-stream
                 :output-stream output-stream))

(defun maybe-expand-memory (computer address)
  (with-slots (memory) computer
    (let ((memory-size (length memory)))
      (when (>= address memory-size)
        (let ((growth-factor (ceiling address memory-size)))
         (setf memory
               (adjust-array memory
                             (+ (* growth-factor memory-size) 1)
                             :initial-element 0)))))))

(defun get-memory-at-address (computer address)
  (with-slots (memory) computer
    (maybe-expand-memory computer address)
    (elt memory address)))
(defun (setf get-memory-at-address) (new-value computer address)
  (with-slots (memory) computer
    (maybe-expand-memory computer address)
    (setf (elt (slot-value computer 'memory) address) new-value)))

(defun get-next-memory (computer)
  (with-slots (instruction-pointer) computer
    (prog1 (get-memory-at-address computer instruction-pointer)
      (incf instruction-pointer))))

(defun peek (computer pointer)
  (get-memory-at-address computer pointer))

(defun poke (computer pointer new-value)
  (setf (get-memory-at-address computer pointer) new-value))

(defun get-next-value (computer mode)
  (let ((value (get-next-memory computer)))
    (ecase mode
      (0 (get-memory-at-address computer value))
      (1 value)
      (2 (get-memory-at-address computer
                                (+ value
                                   (slot-value computer 'reference-base)))))))

(defun get-parameters (computer num-parameters modes)
  (loop
     :for idx :from 0 :below num-parameters
     :collect (get-next-value computer (nth idx modes))))

(defun get-immediate-parameter (computer)
  (get-next-memory computer))

(defun read-input (computer)
  (with-slots (instruction-pointer input-stream) computer
    (when (interactive-stream-p input-stream)
      (format *query-io* "~&(~D)> " instruction-pointer))
    (read input-stream)))

(defun write-output (computer value)
  (with-slots (output-stream) computer
    (format output-stream "~A " value)
    (when (interactive-stream-p output-stream)
      (fresh-line output-stream))))

; (fmakunbound 'execute)
(defgeneric execute (instruction modes computer))

(defun write-to-memory (computer result-location mode value)
  (setf (get-memory-at-address computer
                               (+ result-location
                                  (if (= mode 2) (slot-value computer 'reference-base)
                                      0)))
        value))

(defmacro def-memory-modifier (name modify &key (num-args 2))
  `(defmethod execute ((instruction (eql ,name)) modes (computer computer))
     (let ((args (get-parameters computer ,num-args modes))
           (result-location (get-immediate-parameter computer)))
       (with-slots (state) computer
         (setf state :running)
         ;; fix this
         ;; (get-memory-at-address computer result-location)
         ;; (apply ,modify args)
         (write-to-memory computer result-location (nth 2 modes) (apply ,modify args))
         ))))

(def-memory-modifier :add #'+)
(def-memory-modifier :mul #'*)
(def-memory-modifier :less-than #'(lambda (arg1 arg2) (if (< arg1 arg2) 1 0)))
(def-memory-modifier :eql #'(lambda (arg1 arg2) (if (= arg1 arg2) 1 0)))

(defmacro def-jump-if (name test)
  `(defmethod execute ((instruction (eql ,name)) modes (computer computer))
     (let ((args (get-parameters computer 2 modes)))
       (with-slots (instruction-pointer state) computer
         (setf state :running)
         (when (funcall ,test (first args))
           (setf instruction-pointer (second args)))))))

(def-jump-if :jump-if-true (complement #'zerop))
(def-jump-if :jump-if-false #'zerop)

(defmethod execute ((instruction (eql :inp)) modes (computer computer))
  (let ((result-location (get-immediate-parameter computer)))
    (with-slots (state instruction-pointer) computer
      (handler-case
         (progn  (setf state :running
                 ;; fix this
                 ;; (get-memory-at-address computer result-location)
                 ;; (read-input computer)
                       )
                 (write-to-memory computer result-location (nth 0 modes)
                                  (read-input computer)))
        (end-of-file ()
          (setf state :blocked-input)
          (decf instruction-pointer 2))))))

(defmethod execute ((instruction (eql :out)) modes (computer computer))
  (let ((args (get-parameters computer 1 modes)))
    (with-slots (state) computer
      (write-output computer (car args))
      (setf state :running))))

(defmethod execute ((instruction (eql :set-ref-base)) modes (computer computer))
  (let ((args (get-parameters computer 1 modes)))
    (with-slots (state reference-base) computer
      (setf state :running
            reference-base (+ reference-base (first args))))))

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
                (9 :set-ref-base)
                (99 :halt))
              modes))))

(defun process-instruction (computer)
  (multiple-value-bind (op modes) (get-instruction computer)
   (funcall #'execute op modes computer)))

(defgeneric run-program (computer &optional additional-input))
(defmethod run-program ((computer computer) &optional additional-input)
  ;; cannot run a halted computer...
  (when (eq (get-state computer) :halt) (error 'computer-halted))

  (when additional-input
    (with-slots (input-stream) computer
      (setf input-stream
            (make-concatenated-stream input-stream
                                      ;; because we will be READing from the and
                                      ;; we want the inputs to be distinct.
                                      (make-string-input-stream " ")
                                      additional-input))))
  (loop
     :do (process-instruction computer)
     :until (or (eq (get-state computer) :halt)
                (eq (get-state computer) :blocked-input))
     :finally (return computer)))

(defun compute (initial-memory &key
                                 (input-stream *standard-input*)
                                 (output-stream *standard-output*))
  (run-program
   (make-computer initial-memory input-stream output-stream)))
