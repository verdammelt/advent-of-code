(defpackage #:aoc-2019-16
  (:use :cl))

(in-package #:aoc-2019-16)

(aoc:def-today-suite*)

(defparameter *base-pattern* '(0 1 0 -1))

(defun repeat (n item)
  (loop for i from 0 below n
       collect item))

(defun ones-only (n) (mod n 10))


(defun compute-pattern (digit-num target-length)
  (labels ((compute (digits acc)
             (cond ((> (length acc) target-length) acc)
                   ((null digits) (compute *base-pattern* acc))
                   (t (compute (rest digits)
                               (append acc (repeat digit-num
                                                   (first digits))))))))
    (subseq (rest (compute *base-pattern* (list))) 0 target-length)))

(defun sum-nums (nums) (apply #'+ nums))

(defun compute-phase (input)
  (let* ((input-length (length input))
         (inputs (repeat input-length input))
         (patterns (loop for i from 1 upto input-length
                      collect (compute-pattern i input-length))))
    (mapcar #'ones-only
            (mapcar #'abs
                    (mapcar #'sum-nums
                            (mapcar #'(lambda (i p) (mapcar #'* i p)) inputs patterns))))))

(defun load-signal (file)
  (mapcar #'digit-char-p (coerce (first (aoc:read-data file)) 'list)))

(defun run-n-phases (signal-data num-phases)
  (loop
     for input = signal-data then (compute-phase input)
     for num from 0 below num-phases
     finally (return input)))

(defun n-digits-as-int (digits n)
  (parse-integer (format nil "~{~D~}"(subseq digits 0 n))))

(defun first-8-after-n-phases (input-file)
  (n-digits-as-int (run-n-phases (load-signal input-file) 100) 8))

(defun get-message-offset (signal-data)
  (parse-integer (write-signal (subseq signal-data 0 7) nil)))

(defun compute-message (signal-data
                        &key (forced-offset nil) (repeat-count 10000))
  (let* ((offset (or forced-offset (get-message-offset signal-data)))
         (full-signal (aoc:flatten (repeat repeat-count signal-data)))
         (100th-phase (run-n-phases full-signal 100))
         (message (subseq 100th-phase offset (+ offset 8))))
    (parse-integer (write-signal message nil))))

(defun write-signal (signal-data &optional (stream t))
  (format stream "~{~D~}" signal-data))

;; part 1
(5am:def-test part1 (:suite :aoc-2019-16)
  (5am:is (= 24176176
             (compute-message
              (load-signal (aoc:today-data-pathname "example-after-100-24176176"))
              :forced-offset 0 :repeat-count 1)))
  (5am:is (= 52432133
             (compute-message
              (load-signal (aoc:today-data-pathname "example-after-100-52432133"))
              :forced-offset 0 :repeat-count 1)))
  (5am:is (= 73745418
             (compute-message
              (load-signal (aoc:today-data-pathname "example-after-100-73745418"))
              :forced-offset 0 :repeat-count 1)))
  (5am:is (= 32002835
             (compute-message
              (load-signal (aoc:today-data-pathname))
              :forced-offset 0 :repeat-count 1))))

;; part 2
;; incomplete - above does not terminate quick enough.
;; TODO: complete 2019.16
;; (5am:is (= 84462026 (compute-message (load-signal "./example-with-offset-84462026.txt"))))
