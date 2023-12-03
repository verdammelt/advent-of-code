(defpackage #:aoc-2023-03
  (:use :cl))

(in-package #:aoc-2023-03)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun parse-schematic (schematic)
  "SCHEMATIC is a list of strings which describe a 2d array. It contains numbers
and symbols. #\. is a 'blank'"
  (let ((max-col (length (first schematic)))
        (max-row (length schematic))
        (numbers-with-coords (list))
        (symbols-with-coords (list)))
    (loop for y below max-row
          for row = (elt schematic y) then (elt schematic y)
          do (loop for x below max-col
                   for char = (char row x) then (char row x)
                   with number-accumulator = nil

                   ;; when a digit - accumulate it for later processing
                   when (digit-char-p char)
                     do (push (list char (list x y)) number-accumulator)

                   ;; finish processing a number
                   when (and (not (digit-char-p char)) (not (null number-accumulator)))
                     do (let ((nums (mapcar #'first (reverse number-accumulator)))
                              (coords (mapcar #'second (reverse number-accumulator))))
                          (push (list (parse-integer (coerce nums 'string))
                                      coords)
                                numbers-with-coords)
                          (setf number-accumulator nil))

                   ;; a symbol
                   when (and (char/= char #\.) (not (digit-char-p char)))
                     do (push (list char (list x y)) symbols-with-coords)

                   ;; if we were accumulating a number - finish it.
                   finally (if (not (null number-accumulator))
                               (let ((nums (mapcar #'first (reverse number-accumulator)))
                                     (coords (mapcar #'second (reverse number-accumulator))))
                                 (push (list (parse-integer (coerce nums 'string))
                                             coords)
                                       numbers-with-coords)))))

    (list numbers-with-coords symbols-with-coords)))

(defun neighbors-of (coords)
  (loop for (x y) in coords
        append `((,(1- x) ,y)
                 (,(1- x) ,(1+ y))
                 (,x ,(1+ y))
                 (,(1+ x) ,(1+ y))
                 (,(1+ x) , y)
                 (,(1+ x) ,(1- y))
                 (,x ,(1- y))
                 (,(1- x) ,(1- y)))))

(defun find-part-numbers (parsed-schematic)
  (destructuring-bind (numbers-with-coords symbols-with-coords) parsed-schematic
    (let ((all-symbol-coords (mapcar #'second symbols-with-coords)))
      (loop for (num coords) in numbers-with-coords
            for number-neighbors = (neighbors-of coords) then (neighbors-of coords)
            when (intersection all-symbol-coords number-neighbors :test #'equal)
              collect num))))

(defun part1 (input)
  (aoc:sum (find-part-numbers (parse-schematic input))))

(5am:def-test part1 (:suite :aoc-2023-03)
  (5am:is (= 4361 (part1 +example+)))
  (5am:is (= 539713 (part1 +input+))))

(defun find-gears (parsed-schematic)
  (destructuring-bind (numbers-with-coords symbols-with-coords) parsed-schematic
    (let ((gear-symbols (remove-if-not #'(lambda (c) (char= c #\*))
                                       symbols-with-coords
                                       :key #'first)))
      (loop for possible-gear in gear-symbols
            for neighbors = (neighbors-of (list (second possible-gear)))
              then (neighbors-of (list (second possible-gear)))
            when (progn
                      (let ((nums (remove-if-not
                                   #'(lambda (coords) (intersection coords neighbors :test #'equal))
                                   numbers-with-coords
                                   :key #'second)))
                        (when (= 2 (length nums))
                          (list possible-gear nums))))
              collect it))))

(defun gear-ratio (gear)
  (destructuring-bind (symbol nums) gear
    (declare (ignore symbol))
    (aoc:product (mapcar #'first nums))))

(defun part2 (input)
  (aoc:sum (mapcar #'gear-ratio (find-gears (parse-schematic input)))))

(5am:def-test part2 (:suite :aoc-2023-03)
  (5am:is (= 467835 (part2 +example+)))
  (5am:is (= 84159075 (part2 +input+))))
