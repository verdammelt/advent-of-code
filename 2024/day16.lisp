(defpackage #:aoc-2024-16
  (:use :cl))

(in-package #:aoc-2024-16)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file :post-process #'aoc:lists->2d-array))

(defun parse-maze (data)
  (let ((maze (make-hash-table :test #'equal)))
    (setf (gethash :size maze) (array-dimensions data))
    (aoc:map-2d-array
     #'(lambda (m x y) (let ((pos (aoc:make-coord x y))
                        (char (aref m x y)))
                    (cond ((char= char #\.) (setf (gethash pos maze) t))
                          ((char= char #\E) (setf (gethash pos maze) t
                                                  (gethash :end maze) pos))
                          ((char= char #\S) (setf (gethash pos maze) t
                                                  (gethash :start maze) pos)))))
     data)
    maze))

(defun print-maze (maze)
  (let ((array (make-array (gethash :size maze) :initial-element #\#)))
    (mapcar #'(lambda (pos) (when (listp pos) (setf (aoc:coord-aref array pos) #\.)))
            (alexandria:hash-table-keys maze))
    (setf (aoc:coord-aref array (gethash :start maze)) #\S
          (aoc:coord-aref array (gethash :end maze)) #\E)

    (aoc:print-2d-array array)))

(defparameter +example+
  (parse-maze (read-data (aoc:today-data-pathname "example"))))

(defparameter +example-2+
  (parse-maze (read-data (aoc:today-data-pathname "example-2"))))

(defparameter +input+
  (parse-maze
   (read-data (aoc:today-data-pathname))))

(defun make-node (pos dir) (list pos dir))
(defun node-pos (node) (first node))
(defun node-dir (node) (second node))

(defun dir->coord (dir)
  (case dir
    (:right (aoc:make-coord 0 1))
    (:left (aoc:make-coord 0 -1))
    (:up (aoc:make-coord -1 0))
    (:down (aoc:make-coord 1 0))))

(defun turn-left-and-right (dir)
  (case dir
    (:right '(:up :down))
    (:left '(:down :up))
    (:up '(:left :right))
    (:down '(:right :left))))

(defun neighbors (graph node)
  (append
   (mapcar #'(lambda (dir) (make-node (node-pos node) dir))
           (turn-left-and-right (node-dir node)))
   (list
    (let ((next-pos-in-dir
            (aoc:coord-add (node-pos node) (dir->coord (node-dir node)))))
      (when (gethash next-pos-in-dir graph)
        (make-node next-pos-in-dir (node-dir node)))))))

(defun cost (graph node1 node2)
  (declare (ignore graph))
  (if (eq (node-dir node1) (node-dir node2)) 1 1000))

(defun all-vertexes (graph)
  (mapcan
   #'(lambda (pos) (mapcar #'(lambda (dir) (make-node pos dir)) '(:right :left :up :down)))
   (remove-if-not #'listp (alexandria:hash-table-keys graph))))

(defun end-node-p (node end) (aoc:coord-equal node end))

(defun part1 (input &optional (dirs '(:right :up)))
  (let ((graph input)
        (start-node (make-node (gethash :start input) :right))
        (end-pos (gethash :end input)))
    ;; a node for this path finding algorithm will be (COORD DIR)
    ;; DIR will be one of :RIGHT, :LEFT, :UP, :DOWN

    (apply #'min
           (mapcar #'(lambda (dir)
                       (aoc:dijkstra graph start-node (make-node end-pos dir)
                                     #'neighbors #'cost #'all-vertexes
                                     :test #'end-node-p))
                   dirs))))

(5am:def-test part1 (:suite :aoc-2024-16)
  (5am:is (= 7036 (part1 +example+)))
  (5am:is (= 11048 (part1 +example-2+)))

  ;; this is currently WAY too slow ~7-13 minutes
  ;; (5am:is (= 111480 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2024-16)
  (5am:skip ":aoc-2024-16.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
