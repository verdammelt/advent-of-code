(defpackage #:aoc-2023-17
  (:use :cl))

(in-package #:aoc-2023-17)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file :line-parser #'aoc:number-string->list-of-digits
                      :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun make-node (coord direction step)
  (list coord direction step))
(defun node-coord (node) (first node))
(defun node-dir (node) (second node))
(defun node-step (node) (third node))
(defun node-equal (n1 n2)
  (equal (first n1) (first n2)))

(defun invalid-node-p (graph node)
  (destructuring-bind (max-row max-col) (array-dimensions graph)
    (destructuring-bind ((row col) dir step) node
      (or (> step 3)
          (minusp row) (minusp col)
          (>= row max-row) (>= col max-col)))))

(defparameter *start-node* (make-node (aoc:make-coord 0 0) nil nil))

(defun neighbors (graph node)
  (if (node-equal node *start-node*)
      (list (make-node (aoc:make-coord 0 1) :east 1)
            (make-node (aoc:make-coord 1 0) :south 1))
      (destructuring-bind (max-row max-col) (array-dimensions graph)
        (destructuring-bind ((row col) dir step) node
          (flet ((move-dir (row col dir) (ecase dir
                                           (:north (aoc:make-coord (1- row) col))
                                           (:south (aoc:make-coord (1+ row) col))
                                           (:east (aoc:make-coord row (1- col)))
                                           (:west (aoc:make-coord row (1+ col)))))
                 (turn-left (dir) (ecase dir
                                    (:north :west)
                                    (:south :east)
                                    (:east :north)
                                    (:west :south)))
                 (turn-right (dir) (ecase dir
                                     (:north :east)
                                     (:south :west)
                                     (:east :south)
                                     (:west :north)))
                 (invalid-p (node) (invalid-node-p graph node))
                 (tweak-if-end-node (node)
                   (if (and (= (1- max-row) (aoc:coord-x (node-coord node)))
                            (= (1- max-col) (aoc:coord-y (node-coord node))))
                       (make-node (node-coord node) nil nil)
                       node)))
            (mapcar #'tweak-if-end-node
                    (remove-if #'invalid-p
                               (list (make-node (move-dir row col dir) dir (1+ step))
                                     (make-node (move-dir row col (turn-left dir)) (turn-left dir) 1)
                                     (make-node (move-dir row col (turn-right dir)) (turn-right dir) 1)))))))))

(defun cost (graph node1 node2)
  (declare (ignore node1))
  (destructuring-bind (row col) (node-coord node2)
    (aref graph row col)))

(defun all-vertex (graph)
  (destructuring-bind (max-row max-col) (array-dimensions graph)
    (flet ((invalid-p (node)
             (destructuring-bind ((row col) dir step) node
               (cond ((and (zerop row) (zerop col)) t)
                     ((and (= (1- max-row) row) (= (1- max-col) col)) t)
                     ((and (zerop row) (eq dir :south)) t)
                     ((and (zerop col) (eq dir :west)) t)
                     ((and (= (1- max-row) row) (eq dir :north)) t)
                     ((and (= (1- max-col) col) (eq dir :east)) t)
                     ((and (< row (1- step)) (eq dir :south)) t)
                     ((and (< col (1- step)) (eq dir :west)) t)
                     ((and (> row (- max-row step)) (eq dir :north)) t)
                     ((and (> row (- max-col step)) (eq dir :east)) t)))))
      (let ((all
              (loop for row below max-row
                    append (loop for col below max-col
                                 append (loop for dir in '(:north :south :east :west)
                                              append (loop for step from 1 to 3
                                                           collect (make-node (aoc:make-coord row col)
                                                                              dir step)))))))
        (setf all (remove-if #'invalid-p all))
        (push *start-node* all)
        (push (make-node (mapcar #'1- (array-dimensions graph)) nil nil) all)))))

(defun part1 (input)
  (aoc:dijkstra input
                *start-node*
                (make-node (mapcar #'1- (array-dimensions input)) nil nil)
                #'neighbors
                #'cost
                #'all-vertex
                :test #'node-equal))

(5am:def-test part1 (:suite :aoc-2023-17)
  (5am:is (= 102 (part1 +example+)))
  (5am:skip ":aoc-2023-17.1 not complete - implementation is too slow")
  ;; (5am:is (= -1 (part1 +input+))) ; TODO: [2023-12-17] too slow!
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2023-17)
    (5am:skip ":aoc-2023-17.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
