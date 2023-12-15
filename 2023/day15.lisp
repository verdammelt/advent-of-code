(defpackage #:aoc-2023-15
  (:use :cl))

(in-package #:aoc-2023-15)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file :line-parser #'(lambda (line) (aoc:split-string-on-char #\, line))
                      :post-process #'aoc:flatten))

(defparameter +example+
  (aoc:split-string-on-char #\, "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun hash (str)
  "Hash STR in the following way:

- current = 0
- add ASCII value of character to current
- current = current * 17
- current = rem (current, 256)"

  (loop for c across str
        with current = 0
        do (incf current (char-code c))
        do (setf current (* current 17))
        do (setf current (rem current 256))
        finally (return current)))

(defun part1 (input)
  (aoc:sum (mapcar #'hash input)))

(5am:def-test part1 (:suite :aoc-2023-15)
  (5am:is (= 1320 (part1 +example+)))
  (5am:is (= 506869 (part1 +input+))))

(defparameter *boxes* (make-hash-table :size 256))
(defun clear-boxes () (setf *boxes* (make-hash-table :size 256)))

(defun make-lens (label focal-length)
  (cons label focal-length))
(defun lens-label (lens) (car lens))
(defun lens-length (lens) (cdr lens))
(defun (setf lens-length) (new-value lens)
  (setf (cdr lens) new-value))
(defun lens-focusing-power (lens)
  (destructuring-bind (label . length) lens
    (* (1+ (hash label)) length)))

(defun get-box (label)
  (alexandria:ensure-gethash (hash label) *boxes* (list)))

(defun set-box (label box)
  (setf (gethash (hash label) *boxes*) box))

(defun add-lens-to-box (label focal-length)
  (let* ((box (get-box label))
         (lens (find label box :key #'lens-label :test #'string=)))
    (if lens (setf (lens-length lens) focal-length)
        (setf box (nconc box (list (make-lens label focal-length)))))
    (set-box label box))
  *boxes*)

(defun remove-lens-from-box (label)
  (let ((box (get-box label)))
    (setf box (delete label box :key #'lens-label :test #'string=))
    (set-box label box)
    *boxes*))

(defun make-instruction (box action length)
  (list box action length))
(defun instruction-box (instruction) (first instruction))
(defun instruction-action (instruction) (second instruction))
(defun instruction-length (instruction) (third instruction))
(defun instruction-perform (instruction)
  (destructuring-bind (box action length) instruction
    (ecase action
      (#\= (add-lens-to-box box length))
      (#\- (remove-lens-from-box box)))))

(defun total-focusing-power ()
  (let ((power 0))
    (maphash #'(lambda (box-num lenses)
                 (declare (ignore box-num))
                 (incf power (loop for l in lenses
                                   for idx from 1
                                   sum (* idx (lens-focusing-power l)))))
             *boxes*)
    power))

(defun parse-instruction (inst)
  (let* ((sep-idx (position-if #'(lambda (c) (member c '(#\= #\-) :test #'char=)) inst))
         (box (subseq inst 0 sep-idx))
         (action (char inst sep-idx))
         (length (when (< sep-idx (length inst))
                   (parse-integer (subseq inst (1+ sep-idx)) :junk-allowed t))))
    (make-instruction box action length)))

(defun part2 (input)
  (let ((instructions (mapcar #'parse-instruction input)))
    (clear-boxes)
    (mapcar #'instruction-perform instructions)
    (total-focusing-power)))

(5am:def-test part2 (:suite :aoc-2023-15)
  (5am:is (= 145 (part2 +example+)))
  (5am:is (= 271384 (part2 +input+))))
