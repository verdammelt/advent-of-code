(defpackage #:aoc-2022-10
  (:use :cl))

(in-package #:aoc-2022-10)

(aoc:def-today-suite*)

(defun parse-command (str)
  (let* ((parts (aoc:split-string-on-char #\Space str))
         (instruction (aoc:keywordize (first parts))))
    (ecase instruction
      (:noop (list instruction))
      (:addx (list instruction (parse-integer (second parts)))))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-command))

(defparameter +small+
  (read-data (aoc:today-data-pathname "small")))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defclass computer ()
  ((x :initform 1 :accessor register-x)
   (cycle :initform 1 :accessor cycle)
   (output :initform (list) :accessor output-tape)))

(defgeneric read-output (computer))
(defmethod read-output ((computer computer))
  (reverse (output-tape computer)))

(defun record-registers-to-output (computer)
  (push (list (cycle computer) (register-x computer)) (output-tape computer)))

(defgeneric execute-instruction (computer opcode &rest args))
(defmethod execute-instruction :after ((computer computer) opcode &rest args)
  (declare (ignore opcode args))
  (record-registers-to-output computer))
(defmethod execute-instruction ((computer computer) (opcode (eql :noop)) &rest args)
  (declare (ignore args))
  (incf (cycle computer)))
(defmethod execute-instruction ((computer computer) (opcode (eql :addx)) &rest args)
  (let ((v (first args)))
    (incf (cycle computer) 2)
    (incf (register-x computer) v)))

(defun make-computer ()
  (let ((computer (make-instance 'computer)))
    (record-registers-to-output computer)
    computer))

(defun run-program (computer program)
  (dolist (instruction program)
    (apply #'execute-instruction computer (first instruction) (rest instruction)))
  (read-output computer))

(defun get-x-at-cycle (tape n)
  (let ((idx (1- (position-if #'(lambda (cycle-and-register) (> (first cycle-and-register) n)) tape))))
    (list n (second (nth idx tape)))))

(defun signal-strength (cycle-and-register)
  (aoc:product cycle-and-register))

(defun get-interesting-cycles (tape start repeat)
  (let ((max-cycle (first (first (last tape)))))
    (loop for c from start by repeat below (1+ max-cycle)
          collect (get-x-at-cycle tape c))))

(defun part1 (input)
  (let ((output (run-program (make-computer) input)))
    (aoc:sum
     (mapcar #'signal-strength (get-interesting-cycles output 20 40)))))

(5am:def-test part1 (:suite :aoc-2022-10)
  (5am:is (equal '((1 1) (2 1) (4 4) (6 -1))
           (run-program (make-computer) +small+)))
  (5am:is (= 13140 (part1 +example+)))
  (5am:is (= 13740 (part1 +input+))))

(defun make-crt ()
  (make-array '(6 40) :initial-element #\.))

(defun pixel-on-p (gun-pos sprite-col)
  (<= (1- sprite-col) gun-pos (1+ sprite-col)))

(defun draw-on-crt (crt output)
  (loop for i below (array-total-size crt)
        for (_cycle x) = (get-cycle output (1+ i))
        when (pixel-on-p (mod i (array-dimension crt 1)) x)
          do (setf (row-major-aref crt i) #\#)))

(defun display-crt (crt stream)
  (let ((num-rows (array-dimension crt 0))
        (num-cols (array-dimension crt 1)))
    (loop for r below num-rows
          do (format stream "~A~&"
                     (concatenate 'string (loop for c below num-cols
                                                collect (aref crt r c)))))))

(defun part2 (input &optional (stream t))
  (let ((computer (make-computer))
        (crt (make-crt)))
    (let ((output (run-program computer input)))
      (draw-on-crt crt output)
      (display-crt crt stream))))

(5am:def-test part2 (:suite :aoc-2022-10)
  (5am:is (string=
           (format nil "##..##..##..##..##..##..##..##..##..##..~@
                        ###...###...###...###...###...###...###.~@
                        ####....####....####....####....####....~@
                        #####.....#####.....#####.....#####.....~@
                        ######......######......######......####~@
                        #######.......#######.......#######.....~@
                        ")
           (with-output-to-string (str) (part2 +example+ str))))
  (5am:is (string=
           (format nil "####.#..#.###..###..####.####..##..#....~@
                        ...#.#..#.#..#.#..#.#....#....#..#.#....~@
                        ..#..#..#.#..#.#..#.###..###..#....#....~@
                        .#...#..#.###..###..#....#....#....#....~@
                        #....#..#.#....#.#..#....#....#..#.#....~@
                        ####..##..#....#..#.#....####..##..####.~@
                        ")
           (with-output-to-string (str) (part2 +input+ str)))))
