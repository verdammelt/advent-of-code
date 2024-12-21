(defpackage #:aoc-2024-20
  (:use :cl))

(in-package #:aoc-2024-20)

(aoc:def-today-suite*)

(defun parse-maze (data)
  (let ((maze (make-hash-table :test #'equalp)))
    (setf (gethash :dimensions maze) (array-dimensions data))
    (aoc:map-2d-array
     #'(lambda (m x y)
         (cond ((char= (aref m x y) #\S) (setf (gethash :start maze) (aoc:make-coord x y)
                                               (gethash (aoc:make-coord x y) maze) t))
               ((char= (aref m x y) #\E) (setf (gethash :end maze) (aoc:make-coord x y)
                                               (gethash (aoc:make-coord x y) maze) t))
               ((char= (aref m x y) #\.) (setf (gethash (aoc:make-coord x y) maze) t))))
     data)
    maze))

(defun print-maze (maze)
  (let ((array (make-array (gethash :dimensions maze) :initial-element #\#)))
    (maphash #'(lambda (k v) (when (eq v t) (setf (aoc:coord-aref array k) #\.))) maze)
    (setf (aoc:coord-aref array (gethash :start maze)) #\S
          (aoc:coord-aref array (gethash :end maze)) #\E)
    (aoc:print-2d-array array)))

(defun read-data (file)
  (parse-maze (aoc:read-data file :post-process #'aoc:lists->2d-array)))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defparameter +dirs+ '((-1 0) (1 0) (0 -1) (0 1)))

(defun neighbors (maze node)
  (remove-if-not #'(lambda (pos) (gethash pos maze))
                 (mapcar #'(lambda (dir) (aoc:coord-add node dir)) +dirs+)))

(defun find-path-through-maze (maze)
  (aoc:dijkstra maze (gethash :start maze) (gethash :end maze)
                #'neighbors (constantly 1)
                (constantly (remove-if-not #'listp (alexandria:hash-table-keys maze)))))

(defun find-possible-cheats (maze path)
  (let ((possible-cheats (list))
        (dirs '((-1 0) (1 0) (0 -1) (0 1))))
    (dolist (pos (remove (gethash :end maze) path :test #'equalp) possible-cheats)
      (mapcar #'(lambda (dir)
                  (let* ((next (aoc:coord-add pos dir))
                         (next-next (aoc:coord-add next dir)))
                    (when (and (not (gethash next maze))
                               (gethash next-next maze))
                      (push (list next next-next) possible-cheats))))
              dirs))))

(defun add-cheat (maze cheat)
  (let ((cheated-maze (alexandria:copy-hash-table maze)))
    (setf (gethash (first cheat) cheated-maze) t
          (gethash (second cheat) cheated-maze) t)
    cheated-maze))

(defun part1 (input &optional (count-cheats-matching #'identity))
  (multiple-value-bind (no-cheat-cost no-cheat-path)
      (find-path-through-maze input)
    (count-if
     count-cheats-matching
     (let ((successful-cheats (list)))
       (dolist (cheat (find-possible-cheats input no-cheat-path) successful-cheats)
         (multiple-value-bind (cheat-cost cheat-path)
             (find-path-through-maze (add-cheat input cheat))
           (declare (ignore cheat-path))
           (when (plusp (- no-cheat-cost cheat-cost))
             (push (list (- no-cheat-cost cheat-cost) cheat) successful-cheats))))))))

(defun cheat-with-savings-of (savings)
    (lambda (cost-saving-and-cheat) (= (first cost-saving-and-cheat) savings)))

(defun cheat-with-savings-of-at-least (savings)
  (lambda (cost-saving-and-cheat) (= (first cost-saving-and-cheat) savings)))

(5am:def-test part1 (:suite :aoc-2024-20)
  (5am:is (= 1 (part1 +example+ (cheat-with-savings-of 64))))
  (5am:skip ":aoc-2024-20.1 too slow on real input")
  ;; (5am:is (= -1 (part1 +input+ (cheat-with-savings-of-at-least 100))))
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2024-20)
    (5am:skip ":aoc-2024-20.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
