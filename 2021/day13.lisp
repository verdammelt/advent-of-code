(defpackage #:aoc-2021-13
  (:use :cl))

(in-package #:aoc-2021-13)

(aoc:def-today-suite*)

(defun line-parser-dispatch (line)
  (let* ((fold-prefix "fold along ")
         (fold-prefix-len (length fold-prefix)))
   (cond ((aoc:empty-string-p line) nil)
         ((and (> (length line) fold-prefix-len)
               (string= line fold-prefix :end1 fold-prefix-len))
          (list :fold (aoc:split-string-on-char #\= (subseq line fold-prefix-len))))
         (t (aoc:string-of-numbers->list-of-numbers line #\,)))))

(defun get-max-coords (coords)
  (loop for c in coords
        maximizing (first c) into max-x
        maximizing (second c) into max-y
        finally (return (list max-x max-y))))

(defun make-2d-array-from-coords (coords)
  (reduce #'(lambda (arr coord)
              (setf (aref arr (second coord) (first coord)) t)
              arr)
          coords
          :initial-value (make-array (reverse (mapcar #'1+ (get-max-coords coords)))
                                     :initial-element nil)))

(defun parse-fold (fold)
  (destructuring-bind (label (axis idx)) fold
    (declare (ignore label))
    (list (aoc:keywordize axis) (parse-integer idx))))

(defun collate-data (data)
  (destructuring-bind (coords folds) (split-sequence:split-sequence nil data)
    (pairlis '(:paper :folds)
             (list (make-2d-array-from-coords coords)
                   (mapcar #'parse-fold folds)))))

(defun paper (data) (cdr (assoc :paper data)))
(defun folds (data) (cdr (assoc :folds data)))

(defun print-paper (paper &optional (stream t))
  (format stream
          "~%~{~{~A~}~%~}"
          (loop for y below (array-dimension paper 0)
                collect (loop for x below (array-dimension paper 1)
                              collect (if (aref paper y x) #\# #\.)))))

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'line-parser-dispatch
                 :post-process #'collate-data))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun fold-paper-left-right (paper idx)
  (let ((new-paper (make-array (list (array-dimension paper 0) idx))))
    (loop for x1 from 0 below idx
          for x2 from (1- (array-dimension paper 1)) above idx
          do (loop for y below (array-dimension paper 0)
                   do (setf (aref new-paper y x1) (or (aref paper y x1)
                                                      (aref paper y x2)))))
    new-paper))

(defun fold-paper-top-bottom (paper idx)
  (let ((new-paper (make-array (list idx (array-dimension paper 1)))))
    (loop for y1 from 0 below idx
          for y2 from (1- (array-dimension paper 0)) above idx
          do (loop for x below (array-dimension paper 1)
                   do (setf (aref new-paper y1 x) (or (aref paper y1 x)
                                                      (aref paper y2 x)))))
    new-paper))

(defun fold-paper (paper fold)
  (ecase (first fold)
    (:x (fold-paper-left-right paper (second fold)))
    (:y (fold-paper-top-bottom paper (second fold)))))

;; TODO: common pattern? counting things in arrays?
;; or maybe this is mapping array->list  then counting?
(defun count-dots (paper)
  (loop for i below (array-total-size paper)
        count (row-major-aref paper i)))

(defun part1 (input)
  (count-dots (fold-paper (paper input) (first (folds input)))))

(5am:def-test folding (:suite :aoc-2021-13)
  (5am:is (string=
           "
#####
#...#
#...#
#...#
#####
.....
.....
"
           (print-paper
            (reduce #'fold-paper (folds +example+)
                    :initial-value (paper +example+))
            nil))))

(5am:def-test part1 (:suite :aoc-2021-13)
  (5am:is (= 17 (part1 +example+)))
  (5am:is (= 747 (part1 +input+))))

(defun part2 (input &optional (stream t))
  (print-paper (reduce #'fold-paper (folds input) :initial-value (paper input))
               stream))

(5am:def-test part2 (:suite :aoc-2021-13)
  (5am:is (string=
           "
.##..###..#..#.####.###...##..#..#.#..#.
#..#.#..#.#..#....#.#..#.#..#.#..#.#..#.
#..#.#..#.####...#..#..#.#....#..#.####.
####.###..#..#..#...###..#....#..#.#..#.
#..#.#.#..#..#.#....#....#..#.#..#.#..#.
#..#.#..#.#..#.####.#.....##...##..#..#.
"
           (part2 +input+ nil))))
