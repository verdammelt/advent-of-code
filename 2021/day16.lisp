(defpackage #:aoc-2021-16
  (:use :cl))

(in-package #:aoc-2021-16)

(aoc:def-today-suite*)

(defun parse-transmission (str)
  (parse-integer str :radix 16))

(defun read-data (file)
  (aoc:read-data file :line-parser #'parse-transmission))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example"))
  "Today's example is a set of example inputs rather than a single one.")

(defparameter +example-2+
  (read-data (aoc:today-data-pathname "example-2"))
  "Part 2 has its own set of example inputs")

(defparameter +input+
  (first (read-data (aoc:today-data-pathname))))

(defun integer-length-4-bit-boundary (integer)
  (* 4 (ceiling (integer-length integer) 4)))

(defun read-bits (size idx integer)
  "Read SIZE bits from INTEGER starting at IDX.
(NOTE: this is counting from the most-signifacant bit)"
  (ldb (byte size (- (integer-length-4-bit-boundary integer)
                     idx size))
       integer))

(defclass packet ()
  ((version :initarg :version :reader packet-version)
   (type :initarg :type :reader packet-type)))

(defclass literal (packet)
  ((value :accessor packet-value)))
(defclass operator (packet)
  ((operator :accessor packet-operator
             :initform #'(lambda (&rest args)
                           (declare (ignore args))
                           (error "missing operator!")))
   (operands :accessor packet-operands :initform (list))))

(defmethod print-packet (packet stream)
  (format stream "Version: ~D" (packet-version packet))
  (format stream " Type: ~D" (packet-type packet)))

(defmethod print-object ((object packet) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-packet object stream)))

(defmethod print-object ((object literal) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-packet object stream)
    (format stream " Value: ~D" (packet-value object))))

(defmethod print-object ((object operator) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-packet object stream)
    (format stream " Operator: ~S Operands: ~S"
            (packet-operator object)
            (packet-operands object))))

(defun make-packet (version type)
  (make-instance (case type (4 'literal) (t 'operator)) :version version :type type))

(defgeneric %read-packet (packet transmission idx))
(defmethod %read-packet (packet transmission idx)
  (error "Unkonwn packet object with type ~S" (type-of packet)))
(defmethod %read-packet ((packet packet) transmission idx)
  (declare (ignore transmission idx))
  (error "Unknown packet type ~S (~D)"
         (class-name (class-of packet)) (packet-type packet)))

(defmethod %read-packet ((packet literal) transmission idx)
  (labels ((read-value (transmission idx accumulator)
             (let ((indicator (read-bits 1 idx transmission)))
               (if (zerop indicator)
                   (nreverse (cons (read-bits 4 (+ idx 1) transmission)
                                   accumulator))
                   (read-value transmission (+ idx 5)
                               (cons (read-bits 4 (+ idx 1) transmission)
                                     accumulator)))))
           (to-number (bit-chunks)
             (parse-integer (format nil "~{~4,'0B~}" bit-chunks) :radix 2)))
    (let ((bit-chunks (read-value transmission idx (list))))
      (setf (packet-value packet) (to-number bit-chunks))
      (values packet (+ idx (* (length bit-chunks) 5))))))

(defun greater-than (v1 v2)
  (if (> v1 v2) 1 0))

(defun less-than (v1 v2)
  (if (< v1 v2) 1 0))

(defun equal-to (v1 v2)
  (if (= v1 v2) 1 0))

(defun type->operator (type)
  (ecase type
    (0 #'+)
    (1 #'*)
    (2 #'min)
    (3 #'max)
    (5 #'greater-than)
    (6 #'less-than)
    (7 #'equal-to)))

(defmethod %read-packet ((packet operator) transmission idx)
  (let* ((length-type (read-bits 1 idx transmission))
         (length-size (ecase length-type (0 15) (1 11))))
    (labels ((read-packets-until-length (sub-idx target-idx accumulator)
               (cond ((>= sub-idx target-idx) (values (nreverse accumulator) sub-idx))
                     (t (multiple-value-bind (subpacket new-idx) (read-packet transmission sub-idx)
                          (read-packets-until-length
                           new-idx
                           target-idx
                           (cons subpacket accumulator))))))
             (read-packets-until-count (sub-idx target-count accumulator)
               (cond ((= (length accumulator) target-count) (values (nreverse accumulator) sub-idx))
                     (t (multiple-value-bind (subpacket new-idx) (read-packet transmission sub-idx)
                          (read-packets-until-count
                           new-idx
                           target-count
                           (cons subpacket accumulator)))))))
      (multiple-value-bind (sub-packets new-idx)
          (case length-type
            (0 (read-packets-until-length (+ idx 1 length-size)
                                          (+ idx 1 length-size
                                             (read-bits length-size (+ idx 1) transmission))
                                          (list)))
            (1 (read-packets-until-count (+ idx 1 length-size)
                                         (read-bits length-size (+ idx 1) transmission)
                                         (list))))
        (setf (packet-operator packet) (type->operator (packet-type packet))
         (packet-operands packet) sub-packets)
        (values packet new-idx)))))

(defun read-packet (transmission idx)
  (let ((version (read-bits 3 idx transmission))
        (type (read-bits 3 (+ idx 3) transmission)))
    (%read-packet (make-packet version type) transmission (+ idx 6))))

(5am:def-test reading-packets (:suite :aoc-2021-16)
  (5am:is (= 2021 (packet-value (read-packet (first +example+) 0))))
  (5am:is (equal '(10 20) (mapcar #'packet-value
                                  (packet-operands (read-packet (second +example+) 0)))))
  (5am:is (equal '(1 2 3) (mapcar #'packet-value
                                  (packet-operands (read-packet (third +example+) 0))))))

(defgeneric packet-versions (packet))
(defmethod packet-versions (packet)
  (error "Unkonwn packet object with type ~S" (type-of packet)))
(defmethod packet-versions ((packet packet))
  (list (packet-version packet)))
(defmethod packet-versions ((packet operator))
  (apply #'append (list (packet-version packet))
        (mapcar #'packet-versions (packet-operands packet))))

(defun part1 (input)
  (let* ((packet (read-packet input 0))
         (versions (packet-versions packet)))
    (aoc:sum versions)))

(5am:def-test part1 (:suite :aoc-2021-16)
  (5am:is (= 16 (part1 (fourth +example+))))
  (5am:is (= 12 (part1 (fifth +example+))))
  (5am:is (= 23 (part1 (sixth +example+))))
  (5am:is (= 31 (part1 (seventh +example+))))
  (5am:is (= 943 (part1 +input+))))

(defgeneric eval-packet (packet))
(defmethod eval-packet (packet)
  (error "Unkonwn packet object with type ~S" (type-of packet)))
(defmethod eval-packet ((packet literal))
  (packet-value packet))

(defmethod eval-packet ((packet operator))
  (apply (packet-operator packet)
         (mapcar #'eval-packet (packet-operands packet))))

(defun part2 (input &optional (idx 0))
  (eval-packet (read-packet input idx)))

(5am:def-test part2 (:suite :aoc-2021-16)
  (5am:is (= 3 (part2 (first +example-2+))))
  ;; TODO: fix this leading-zero bug.
  ;; due to a bug related to leading zeros in the programs we have to provide a
  ;; negative starting index. It is (* N -4) where N is the number of leading zeros.
  (5am:is (= 54 (part2 (second +example-2+) -4)))
  (5am:is (= 7 (part2 (third +example-2+))))
  (5am:is (= 9 (part2 (fourth +example-2+))))
  (5am:is (= 1 (part2 (fifth +example-2+))))
  (5am:is (= 0 (part2 (sixth +example-2+))))
  (5am:is (= 0 (part2 (seventh +example-2+))))
  (5am:is (= 1 (part2 (eighth +example-2+))))
  (5am:is (= 167737115857 (part2 +input+))))
