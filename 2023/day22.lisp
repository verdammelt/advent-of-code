(defpackage #:aoc-2023-22
  (:use :cl))

(in-package #:aoc-2023-22)

(aoc:def-today-suite*)

(defun parse-block (str)
  (mapcar #'(lambda (s) (aoc:string-of-numbers->list-of-numbers s :delimiters #\,))
          (aoc:split-string-on-char #\~ str)))

(defun read-data (file)
  (aoc:read-data file :line-parser #'parse-block))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun fill-out-blocks (block)
  (destructuring-bind (beg end) block
      (loop for x from (first beg) to (first end)
            append (loop for y from (second beg) to (second end)
                          append (loop for z from (third beg) to (third end)
                                       collect (list x y z))))))

(defun fill-out-all-blocks (blocks)
  (mapcar #'fill-out-blocks blocks))

(defun print-elevation (cubes axis)
  "CUBES is a list of cubes not blocks!"
  (flet ((z (cube) (third cube))
         (other (cube) (if (eq axis :x) (first cube) (second cube))))
    (let* ((max-z (reduce #'max cubes :key #'z))
           (max-axis (reduce #'max cubes :key #'other))
           (map (make-array (list (1+ max-z) (1+ max-axis)) :initial-element #\.)))
      (reduce #'(lambda (map cube) (setf (aref map (- max-z (z cube)) (other cube)) #\#) map)
              cubes
              :initial-value map))))

(defun print-z-slice (cubes z)
  (let ((max-x (reduce #'max cubes :key #'first))
        (max-y (reduce #'max cubes :key #'second)))
    (reduce #'(lambda (map cube) (setf (aref map (first cube) (second cube)) #\#) map)
            (remove-if-not #'(lambda (cube) (= z (third cube))) cubes)
            :initial-value (make-array (list (1+ max-x) (1+ max-y)) :initial-element #\.))))

(defun blocks->cubes (blocks)
  (aoc:flatten blocks))

(defun on-ground-p (block)
  (some #'(lambda (cube) (= (third cube) 1)) block))

(defun supported-p (block blocks)
  "Returns T is BLOCK is resting on another BLOCK or the ground (z=0)."
  (or (on-ground-p block)
      (some #'(lambda (cube) (find (list (first cube) (second cube) (1- (third cube)))
                              (blocks->cubes (remove block blocks :test #'equal))
                              :test #'equal))
            block)))

(defun move-cube-down (cube)
  (list (first cube) (second cube) (1- (third cube))))

(defun move-block-down (block)
  (mapcar #'move-cube-down block))

(defun partition (pred seq)
  (let ((true-seq (list))
        (false-seq (list)))
    (mapc
     #'(lambda (el) (if (funcall pred el) (push el true-seq) (push el false-seq)))
          seq)
    (values (nreverse true-seq) (nreverse false-seq))))

(defun collapse (blocks)
  (multiple-value-bind (supported unsupported)
      (partition #'(lambda (b) (supported-p b blocks)) blocks)
    (if unsupported
        (collapse (append supported (mapcar #'move-block-down unsupported)))
        supported)))

(defun part1 (input)
  (let ((settled (collapse (fill-out-all-blocks input))))
    settled))

(5am:def-test part1 (:suite :aoc-2023-22)
  (5am:skip ":aoc-2023-22.1 not implemented")
  ;; (5am:is (= -1 (part1 +example+)))
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2023-22)
  (5am:skip ":aoc-2023-22.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
