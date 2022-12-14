(defpackage #:aoc-2022-12
  (:use :cl))

(in-package #:aoc-2022-12)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun extract-start-end (map)
  "MAP contains #\S for start and #\E for end, extract the coordinates for these
and also update the map to contain #\a for start and #\z for end."
  (let (start end)
    (flet ((process-cell (arr row col)
             (cond ((char= #\S (aref arr row col)) (setf start (list row col)) #\a)
                   ((char= #\E (aref arr row col)) (setf end (list row col)) #\z)
                   (t (aref arr row col)))))
      (let ((new-map (aoc:map-2d-array #'process-cell map)))
        (values start end new-map)))))

(defun get-node (array row col)
  (destructuring-bind (max-row max-col) (array-dimensions array)
    (cond ((or (minusp row) (minusp col) (>= row max-row) (>= col max-col)) nil)
          (t (aref array row col)))))

(defun char-minus (n1 n2)
  "The Common Lisp specification does not assume ASCII; but for now we will"
  (- (char-int n1)
     (char-int n2)))

(defun get-cost (n1 n2)
  (if (and n1 n2) (char-minus n2 n1)))

(defun neighbors (graph node)
  "GRAPH is a 2d array and NODE is a list of (row, col). Evaluates to all valid
neighbors of NODE"
  (destructuring-bind (row col) node
    (let ((here (get-node graph row col))
          (up (list (1- row) col))
          (down (list (1+ row) col))
          (left (list row (1- col)))
          (right (list row (1+ col))))
      (flet ((valid-neighbor-p (there) (let ((cost (get-cost here (apply #'get-node graph there))))
                                         (and cost (< cost 2)))))
        (remove-if-not #'valid-neighbor-p (list up down left right))))))

(defun cost (graph node1 node2)
  "The COST of every valid node is 1."
  (declare (ignore graph node1 node2))
  1)

(defun all-vertexes (graph)
  (loop for row below (array-dimension graph 0)
        append (loop for col below (array-dimension graph 1)
                     collect (list row col))))
(defun part1 (input)
  (multiple-value-bind (start end map) (extract-start-end input)
    (multiple-value-bind (length _path)
        (aoc:dijkstra map start end #'neighbors #'cost #'all-vertexes)
      (declare (ignore _path))
      length)))

(5am:def-test part1 (:suite :aoc-2022-12)
  (5am:is (= 31 (part1 +example+)))
  (5am:is (= 352 (part1 +input+))))

(defun all-vertexes-with-a (graph)
               (remove-if-not #'(lambda (node) (char= #\a (apply #'get-node graph node)))
                              (all-vertexes graph)))

;; TODO: can it be sped up for +input+? takes ~3 minutes!
(defun part2 (input)
  (multiple-value-bind (_start end map) (extract-start-end input)
    (declare (ignore _start))
    (do ((starts (all-vertexes-with-a map) (rest starts))
         (min most-positive-fixnum))
        ((null starts) min)
      (let ((length (aoc:dijkstra map (first starts) end #'neighbors #'cost #'all-vertexes)))
        (when length (setf min (min length min)))))))

(5am:def-test part2 (:suite :aoc-2022-12)
  (5am:is (= 29 (part2 +example+)))
  (5am:skip "(5am:is (= 345 (part2 +input+))) ;; [2022.12] because it takes ~~3 min"))
