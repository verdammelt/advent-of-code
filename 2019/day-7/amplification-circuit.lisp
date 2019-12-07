(load "../file-utils")
(load "../computer")

(defpackage :amplification-curcuit
  (:use :common-lisp))

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
         :expected 65210)))

(defun all-phase-setting-choices ()
  (let ((result (list)))
    (dotimes (a 5)
      (dotimes (b 5)
        (dotimes (c 5)
          (dotimes (d 5)
            (dotimes (e 5)
              (when (/= a b c d e)
                (push (list a b c d e) result)))))))
    result))

(defclass amplifier (computer:computer)
  ((phase :initarg :phase)))

(defun create-amplifier (program phase-setting)
  (make-instance 'amplifier
                 :memory (copy-seq program)
                 :input-stream
                 (make-string-input-stream (format nil "~D" phase-setting))
                 :output-stream
                 (make-string-output-stream)
                 :phase phase-setting))

(defun run-with-input (input computer)
  (computer:get-output
   (computer:run-program computer (make-string-input-stream input))))

(defun run-program-on-amplifiers-with-phases (program phase-settings)
  (flet ((create-amplifier (phase) (create-amplifier program phase)))
    (reduce #'run-with-input
            (mapcar #'create-amplifier phase-settings)
            :initial-value "0")))

(defun find-best-output (program)
  (flet ((get-phases-output (phases)
           (list phases
                 (parse-integer
                  (run-program-on-amplifiers-with-phases program phases)))))
    (first
     (sort (mapcar #'get-phases-output (all-phase-setting-choices))
           #'> :key #'second))))
