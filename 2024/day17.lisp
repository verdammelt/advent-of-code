(defpackage #:aoc-2024-17
  (:use :cl))

(in-package #:aoc-2024-17)

(aoc:def-today-suite*)

(defun parse-machine (data)
  (list :a (third (first data))
        :b (third (second data))
        :c (third (third data))
        :instructions (cdr (fifth data))
        :ip 0
        :output (list)))

(defun read-data (file)
  (aoc:read-data
   file
   :line-parser #'(lambda (str) (aoc:string-of-numbers->list-of-numbers
                            str :delimiters '(#\: #\, #\Space) :junk-allowed t))
   :post-process #'parse-machine))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun literal-operand (operand) operand)

(defun combo-operand (operand machine)
  (cond ((<= operand 3) operand)
        ((= operand 4) (getf machine :a))
        ((= operand 5) (getf machine :b))
        ((= operand 6) (getf machine :c))
        ((= operand 7) (error "Reserved COMBO OPERAND: 7"))
        (t (error "Unknown COMBO OPERAND: ~A" t))))

(defun adv (operand machine)
  "Divide REGISTER-A by (EXPT 2 OPERAND) and store in REGISTER-A"
  (let ((operand (combo-operand operand machine)))
    (setf (getf machine :a)
          (truncate (/ (getf machine :a) (expt 2 operand)))))
  (+ (getf machine :ip) 2))

(defun bxl (operand machine)
  "XOR REGISTER-B with OPERAND and store in REGISTER-B"
  (let ((operand (literal-operand operand)))
    (setf (getf machine :b)
          (logxor (getf machine :b) operand)))
  (+ (getf machine :ip) 2)  )

(defun bst (operand machine)
  "MOD 8 OPERAND and store in REGISTER-B"
  (let ((operand (combo-operand operand machine)))
    (setf (getf machine :b) (mod operand 8)))
  (+ (getf machine :ip) 2)  )

(defun jnz (operand machine)
  "If REGISTER-A is zero: advance INSTRUCTION-POINTER by 2 as normal;
Else set INSTRUCTION-POINTER to value of OPERAND."
  (let ((operand (literal-operand operand)))
    (if (zerop (getf machine :a))
        (+ (getf machine :ip) 2)
        operand)))

(defun bxc (operand machine)
  "XOR REGISTER-B and REGISTER-C and store in REGISTER-B. Ignores OPERAND"
  (declare (ignore operand))
  (setf (getf machine :b) (logxor (getf machine :b) (getf machine :c)))
  (+ (getf machine :ip) 2))

(defun out (operand machine)
  (let ((operand (combo-operand operand machine)))
    (push (mod operand 8) (getf machine :output)))
  (+ (getf machine :ip) 2))

(defun bdv (operand machine)
  "Divide REGISTER-A by (EXPT 2 OPERAND) and store in REGISTER-B"
  (let ((operand (combo-operand operand machine)))
    (setf (getf machine :b)
          (truncate (/ (getf machine :a) (expt 2 operand)))))
  (+ (getf machine :ip) 2))

(defun cdv (operand machine)
  "Divide REGISTER-A by (EXPT 2 OPERAND) and store in REGISTER-C"
  (let ((operand (combo-operand operand machine)))
    (setf (getf machine :c)
          (truncate (/ (getf machine :a) (expt 2 operand)))))
  (+ (getf machine :ip) 2))

(defun op-code-lookup (op-code)
  (nth op-code (list #'adv #'bxl #'bst #'jnz #'bxc #'out #'bdv #'cdv)))

(defun run-program (machine)
  ;; (format t "~S~&" machine)
  (flet ((get-op-code (machine)
           (nth (getf machine :ip) (getf machine :instructions)))
         (get-operand (machine)
           (nth (1+ (getf machine :ip)) (getf machine :instructions))))
   (do ((op-code (get-op-code machine) (get-op-code machine))
        (operand (get-operand machine) (get-operand machine)))
       ((>= (getf machine :ip) (length (getf machine :instructions)))
        machine)

     ;; (format t "~D: (~S ~S) = " (getf machine :ip)
     ;;         (op-code-lookup op-code) operand)

     (setf (getf machine :ip)
           (funcall (op-code-lookup op-code) operand machine))

     ;; (format t "~S~&" machine)

     )))

(defun part1 (input)
  (let ((machine (run-program (copy-seq input))))
    (format nil "~{~D~^,~}" (reverse (getf machine :output)))))

(5am:def-test part1 (:suite :aoc-2024-17)
  (5am:is (string= "4,6,3,5,6,3,5,2,1,0" (part1 +example+)))
  (5am:is (string= "6,7,5,2,1,3,5,1,7" (part1 +input+))))

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2024-17)
  (5am:skip ":aoc-2024-17.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
