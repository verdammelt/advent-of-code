(defpackage #:aoc-2021-24
  (:use :cl))

(in-package #:aoc-2021-24)

(aoc:def-today-suite*)

(defun parse-alu-command (str)
  "Parse the ALU command which is always of the form:
CMD VAR or CMD VAR VAR-OR-NUM"
  (destructuring-bind (cmd a &optional b)
      (aoc:split-string-on-char #\Space str)
    (labels ((handle-optional-var-or-num (x)
               (cond ((null x) x)
                     ((ignore-errors (parse-integer x))
                      (parse-integer x))
                     (t (aoc:keywordize x)))))
      (list (aoc:keywordize cmd) (aoc:keywordize a) (handle-optional-var-or-num b)))))

(defun read-data (file)
  (aoc:read-data file :line-parser #'parse-alu-command))

(defparameter +example-1+
  (read-data (aoc:today-data-pathname "example-1")))

(defparameter +example-2+
  (read-data (aoc:today-data-pathname "example-2")))

(defparameter +example-3+
  (read-data (aoc:today-data-pathname "example-3")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defclass alu ()
  ((registers :initform (reverse (pairlis '(:w :x :y :z) '(0 0 0 0))))
   (input-stream :initarg :input-stream)))

(defmethod print-object ((object alu) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "REG: ~A" (slot-value object 'registers))))

(defun get-alu-register (alu r)
  (cdr (assoc r (slot-value alu 'registers))))

(defun (setf get-alu-register) (v alu r)
  (setf (cdr (assoc r (slot-value alu 'registers))) v))

(defun read-input-stream (input-stream)
  (cond ((or (eq input-stream *standard-input*)
             (eq input-stream *query-io*))
         (format *query-io* "~&WAITING FOR INPUT...")
         (values (read input-stream) input-stream))
        ((streamp input-stream)
         (values (read input-stream) input-stream))
        ((listp input-stream)
         (if (zerop (length input-stream))
             (error "END OF INPUT STREAM")
             (values (first input-stream) (rest input-stream))))))


(defun get-val-or-register (alu x)
  (if (keywordp x) (get-alu-register alu x) x))

(defgeneric execute-command (alu cmd arg1 arg2))
(defmethod execute-command (alu (cmd (eql :inp)) arg1 arg2)
  (multiple-value-bind (value stream)
      (read-input-stream (slot-value alu 'input-stream))
    (setf (get-alu-register alu arg1) value
          (slot-value alu 'input-stream) stream)
    alu))
(defmethod execute-command (alu (cmd (eql :add)) arg1 arg2)
  (setf (get-alu-register alu arg1)
        (+ (get-alu-register alu arg1)
           (get-val-or-register alu arg2)))
  alu)
(defmethod execute-command (alu (cmd (eql :mul)) arg1 arg2)
  (setf (get-alu-register alu arg1)
        (* (get-alu-register alu arg1)
           (get-val-or-register alu arg2)))
  alu)
(defmethod execute-command (alu (cmd (eql :div)) arg1 arg2)
  (setf (get-alu-register alu arg1)
        (floor (get-alu-register alu arg1)
               (get-val-or-register alu arg2)))
  alu)
(defmethod execute-command (alu (cmd (eql :mod)) arg1 arg2)
  (setf (get-alu-register alu arg1)
        (mod (get-alu-register alu arg1)
             (get-val-or-register alu arg2)))
  alu)
(defmethod execute-command (alu (cmd (eql :eql)) arg1 arg2)
  (let ((val1 (get-alu-register alu arg1))
        (val2 (get-val-or-register alu arg2)))
    (setf (get-alu-register alu arg1)
          (if (= val1 val2) 1 0))
    alu))

(defun run-program (alu program)
  (reduce
   #'(lambda (alu instruction) (apply #'execute-command alu instruction))
   program
   :initial-value alu))

(5am:def-test run-program (:suite :aoc-2021-24)
  (5am:is (= -10 (get-alu-register
                  (run-program (make-instance 'alu :input-stream '(10)) +example-1+)
                  :x)))
  (5am:is (= 1 (get-alu-register
                (run-program (make-instance 'alu :input-stream '(2 6)) +example-2+)
                :z)))
  (5am:is (= 0 (get-alu-register
                (run-program (make-instance 'alu :input-stream '(2 5)) +example-2+)
                :z))))

(defun finishes-with-zero-z-p (input program)
  (if (member 0 input) nil ;; number contains a 0 so not a valid input
      (let ((alu (make-instance 'alu :input-stream input)))
        (run-program alu program)
        (zerop (get-alu-register alu :z)))))

(defun number->digits (n) (map 'list #'digit-char-p (format nil "~D" n)))

(defun part1 (input)
  (format t "BEG: ~D~%" (get-universal-time))
  (let ((model-number
          (loop for i from (1- (expt 10 15)) downto (expt 10 14)
                until (finishes-with-zero-z-p (number->digits i) input)

                when (zerop (mod i (expt 10 6)))
                  do (progn (format t "~D.." i) (finish-output t))

                when (zerop (mod i (* 5 (expt 10 6))))
                  do (fresh-line t)

                finally (return i))))
    (list model-number
          (finishes-with-zero-z-p (number->digits model-number) input))
    (format t "~&END: ~D~%" (get-universal-time))))

(5am:def-test part1 (:suite :aoc-2021-24)
  (5am:skip ":aoc-2021-24.1 not implemented")
  ;; (5am:is (= -1 (part1 +example+)))
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2021-24)
  (5am:skip ":aoc-2021-24.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
