(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../file-utils")
  (load "../string-utils")
  (load "../computer"))

; (delete-package (find-package :amplification-curcuit))
(defpackage :amplification-curcuit
  (:use :common-lisp)
  (:export :find-best-output
           :*amplifier-controller-software*))

(in-package :amplification-curcuit)

(defun csv->number-list (str)
  (mapcar #'parse-integer (string-utils:split str #\,)))

(defparameter *amplifier-controller-software*
  (csv->number-list (first (file-utils:read-lines "./input.txt"))))

(defparameter *test-programs*
  (list
   (list :program
         (csv->number-list
                       "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
         :phase-sequence (csv->number-list "4,3,2,1,0")
         :expected 42310)
   (list :program
         (csv->number-list
          "3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0")
         :phase-sequence (csv->number-list "0,1,2,3,4")
         :expected 54321)
   (list :program
         (csv->number-list
          "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
         :phase-sequence (csv->number-list "1,0,4,3,2")
         :expected 65210)
   (list :program
         (csv->number-list "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
         :phase-sequence (csv->number-list "9,8,7,6,5")
         :expected 139629729)
   (list :program
         (csv->number-list "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
         :phase-sequence (csv->number-list "9,7,8,5,6")
         :expected 18216)))

(defun all-phase-setting-choices (&optional (base-num 0))
  (let ((result (list)))
    (flet ((plus-base (n) (+ base-num n)))
      (dotimes (a 5)
        (dotimes (b 5)
          (dotimes (c 5)
            (dotimes (d 5)
              (dotimes (e 5)
                (when (/= a b c d e)
                  (push (mapcar #'plus-base (list a b c d e))
                        result))))))))
    result))

(defclass amplifier (computer:computer)
  ((phase :initarg :phase)
   (name :initarg :name
         :initform "<unknown>"
         :reader get-name)))

(defmethod print-object ((object amplifier) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (computer::state computer::instruction-pointer name phase) object
      (format stream "(NAME:~A PHASE:~A STATE:~A PC:~A)"
              name phase computer::state computer::instruction-pointer))))

(defun create-amplifier (program name phase-setting)
  (make-instance 'amplifier
                 :name name
                 :memory program
                 :input-stream
                 (make-string-input-stream (format nil "~D" phase-setting))
                 :output-stream
                 (make-string-output-stream)
                 :phase phase-setting))

(defun run-to-completion (amplifiers initial-input)
  (flet ((get-comp (n) (nth n amplifiers))
         (make-input (str) (make-string-input-stream str)))
    (do* ((computer-idx 0 (mod (1+ computer-idx) (length amplifiers)))
          (active (get-comp computer-idx) (get-comp computer-idx))
          (final (get-comp (1- (length amplifiers))))
          (input initial-input)
          (iteration 0 (incf iteration)))

         ((eq (computer:get-state final) :halt) input)

      ;; (format t "~&~D: RUNNING: ~A INPUT: ~A" iteration active input)
      (computer:run-program active (make-input input))
      (let ((output (computer:get-output active)))
       ;; (format t " => ~A (~A)~%" output (computer:get-state active))
        (setf input output)))))


(defun n-letters (n)
    "Returns N capital letters - if N is big enough it is possible to get
     non-letters as this is just indexing into the characters table."
    (loop
       :for i :from 0 :below n
       :collect (format nil "~A" (code-char (+ i (char-code #\A))))))

(defun run-program-on-amplifiers-with-phases (program phase-settings)
  (flet ((create-amplifier (name phase) (create-amplifier program name phase)))
    (let ((amplifiers (mapcar #'create-amplifier
                              (n-letters (length phase-settings))
                              phase-settings)))
      (run-to-completion amplifiers "0"))))

(defun find-best-output (program &optional (base-phase 0))
  (flet ((get-phases-output (phases)
           (list phases
                 (parse-integer
                  (run-program-on-amplifiers-with-phases program phases)))))
    (first
     (sort (mapcar #'get-phases-output (all-phase-setting-choices base-phase))
           #'> :key #'second))))

; => (:part1 34686 :part2 36384144)
