(defpackage #:aoc-2020-14
  (:use :cl #:aoc))

(in-package #:aoc-2020-14)

(aoc:def-today-suite*)

(defun str-starts-with-p (str prefix)
  (string= (subseq str 0 (length prefix)) prefix))

(defun parse-mask-line (line)
  "Parse a LINE of form: 'mask = <mask>'"
  (let ((value (map 'vector #'digit-char-p
                    (string-trim '(#\Space)
                                 (second (split-sequence:split-sequence #\= line))))))
    (list :mask value)))

(defun parse-mem-line (line)
  (let* ((parts (mapcar #'(lambda (s) (string-trim '(#\Space) s))
                        (split-sequence:split-sequence #\= line)))
         (address (parse-integer (first parts) :start (length "mem[")
                                               :junk-allowed t))
         (value (parse-integer (second parts))))
    (list :mem address value)))

(defun parse-memory-command (line)
  (cond ((str-starts-with-p line "mask") (parse-mask-line line))
        ((str-starts-with-p line "mem") (parse-mem-line line))
        (t (error "Unknown command input: ~S" line))))

(defparameter +example+ (read-data (today-data-pathname "example")
                                   :line-parser #'parse-memory-command))
(defparameter +example-2+ (read-data (today-data-pathname "example-2")
                                     :line-parser #'parse-memory-command))
(defparameter +input+ (read-data (today-data-pathname)
                                 :line-parser #'parse-memory-command))

(defclass machine ()
  ((mask :accessor mask :initform (make-array '(36) :initial-element NIL))
   (memory :reader memory :initform (make-hash-table))))

(defun apply-mask (mask input)
  (let ((mask (reverse mask)))
    (do ((i 0 (1+ i)))
        ((>= i (length mask)) input)
      (when (elt mask i)
        (setf input (dpb (elt mask i) (byte 1 i) input))))))

(defun execute-v1 (machine command)
  "Performs COMMAND on MACHINE. Returns an object of type MACHINE.
It may destructively modify its MACHINE parameter."
  (destructuring-bind (command &rest args) command
    (ecase command
      (:mask (setf (mask machine) (first args)))
      (:mem (setf (gethash (first args) (memory machine))
                  (apply-mask (mask machine) (second args)))))
    machine))

(defun run-program (tape executor)
  (reduce executor tape :initial-value (make-instance 'machine)))

(defun memory-sum (input executor)
  (let ((machine (run-program input executor))
        (sum 0))
    (maphash #'(lambda (k v) (declare (ignore k)) (incf sum v)) (memory machine))
    sum))


(defun part1 (input)
  (memory-sum input #'execute-v1))

(5am:def-test part1 (:suite :aoc-2020-14)
  (5am:is (= 165 (part1 +example+)))
  (5am:is (= 5055782549997 (part1 +input+))))

(defun int->vec (int num-bits)
  (do ((vec (make-array (list num-bits) :initial-element 0))
       (i 0 (1+ i)))
      ((>= i num-bits) (reverse vec))
    (setf (elt vec i) (ldb (byte 1 i) int))))

(defun vec->int (vec)
  (let ((vec (reverse vec)))
    (do ((int 0)
         (i 0 (1+ i)))
        ((>= i (length vec)) int)
      (setf int (dpb (elt vec i) (byte 1 i) int)))))

(defun apply-mask-v2 (int-vec mask)
  (do ((i 0 (1+ i))
       (new-vec (copy-seq int-vec)))
      ((>= i (length mask)) new-vec)
    (setf (elt new-vec i)
          (case (elt mask i)
            (0 (elt new-vec i))
            (1 1)
            (nil nil)))))

(defun set-elt (v i new)
  (setf (elt v i) new)
  v)

(defun affected-memory-addrs (addr mask)
  (do ((addrs (list (apply-mask-v2 (int->vec addr (length mask)) mask)))
       (i 0 (1+ i)))
      ((>= i (length mask)) (mapcar #'vec->int addrs))

    (setf addrs (reduce
                 #'(lambda (as a)
                     (let ((item (elt a i)))
                       (if item (push a as)
                           (progn (push (set-elt a i 0) as)
                                  (push (set-elt (copy-seq a) i 1) as)))))
                 addrs :initial-value (list)))))

(defun execute-v2 (machine command)
  "Performs COMMAND on MACHINE. Returns an object of type MACHINE.
It may destructively modify its MACHINE parameter."
  (destructuring-bind (command &rest args) command
    (ecase command
      (:mask (setf (mask machine) (first args)))
      (:mem (mapcar #'(lambda (addr) (setf (gethash addr (memory machine)) (second args)))
                    (affected-memory-addrs (first args) (mask machine)))))
    machine))

(defun part2 (input)
  (memory-sum input #'execute-v2))

(5am:def-test part2 (:suite :aoc-2020-14)
  (5am:is (= 208 (part2 +example-2+)))
  ; (5am:is (= xxx (part2 +input+)))
)
