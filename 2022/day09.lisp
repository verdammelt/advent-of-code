(defpackage #:aoc-2022-09
  (:use :cl))

(in-package #:aoc-2022-09)

(aoc:def-today-suite*)

(defun parse-move (str)
  (let ((dir-and-num (aoc:split-string-on-char #\Space str)))
    (list (aoc:keywordize (first dir-and-num))
          (parse-integer (second dir-and-num)))))

(defun move-dir (move) (first move))
(defun move-num (move) (second move))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-move))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +example-2+
  (read-data (aoc:today-data-pathname "example-2")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun make-coord (x y) (cons x y))
(defun coord-equal (c1 c2) (equal c1 c2))
(defun coord-x (coord) (car coord))
(defun coord-y (coord) (cdr coord))
(defun coord-touching (c1 c2)
  (or (coord-equal c1 c2)
      (and (>= 1 (abs (- (coord-x c1) (coord-x c2))))
           (>= 1 (abs (- (coord-y c1) (coord-y c2)))))))

(defun up (coord) (make-coord (1+ (coord-x coord))
                              (coord-y coord)))
(defun down (coord) (make-coord (1- (coord-x coord))
                                (coord-y coord)))
(defun left (coord) (make-coord (coord-x coord)
                                (1- (coord-y coord))))
(defun right (coord) (make-coord (coord-x coord)
                                 (1+ (coord-y coord))))

(defun make-n-knot-rope (num-knots)
  (make-list num-knots :initial-element (make-coord 0 0)))
(defun make-rope (&rest knots) knots)
(defun rope-head (rope) (first rope))
(defun rope-followers (rope) (rest rope))
(defun rope-tail (rope) (first (last rope)))

(defun move-head (head dir)
  (ecase dir
    (:L (left head))
    (:R (right head))
    (:U (up head))
    (:D (down head))))

(defun move-follower (follower new-head)
  (let* ((l/r (if (> (coord-y follower) (coord-y new-head)) #'left #'right))
         (u/d (if (> (coord-x follower) (coord-x new-head)) #'down #'up)))
    (cond ((coord-touching follower new-head) follower)
          ((= (coord-x follower) (coord-x new-head)) (funcall l/r follower))
          ((= (coord-y follower) (coord-y new-head)) (funcall u/d follower))
          (t (funcall u/d (funcall l/r follower))))))

(defun move-followers (followers new-head)
  (let ((head new-head)
        (result (list)))
    (dolist (follower followers)
      (setf head (move-follower follower head))
      (push head result))
    (reverse result)))

(defun move-rope-once (rope dir)
  (let* ((new-head (move-head (rope-head rope) dir))
         (new-followers (move-followers (rope-followers rope) new-head)))
    (apply #'make-rope new-head new-followers)))

(defun move-rope (initial-rope movement)
  (let ((rope initial-rope)
        (trail (list initial-rope))
        (dir (move-dir movement))
        (num (move-num movement)))
    (dotimes (n num)
      (let ((new-rope (move-rope-once rope dir)))
        (push new-rope trail)
        (setf rope new-rope)))
    (reverse trail)))

(defun get-trail (initial-rope moves)
  (let ((rope initial-rope)
        (trail nil))
    (dolist (move moves)
      (let ((rope-trail (move-rope rope move)))
        (setf trail (append trail rope-trail)
              rope (first (last rope-trail)))))
    trail))

(defun count-tail-positions (rope movements)
  (length (remove-duplicates
           (mapcar #'rope-tail (get-trail rope movements))
           :test #'coord-equal)))

(defun part1 (input)
  (count-tail-positions (make-n-knot-rope 2) input))

(5am:def-test part1 (:suite :aoc-2022-09)
  (5am:is (= 13 (part1 +example+)))
  (5am:is (= 6376 (part1 +input+))))

(defun part2 (input)
  (count-tail-positions (make-n-knot-rope 10) input))

(5am:def-test part2 (:suite :aoc-2022-09)
  (5am:is (= 1 (part2 +example+)))
  (5am:is (= 36 (part2 +example-2+)))
  (5am:is (= 2607 (part2 +input+))))
