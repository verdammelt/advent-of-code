(defpackage #:aoc-2020-11
  (:use :cl #:aoc #:aoc-2020/utils))

(in-package #:aoc-2020-11)
(aoc:def-today-suite*)

(defun valid-coordinate (m x y)
  (and (not (minusp x))
       (not (minusp y))
       (< x (array-dimension m 0))
       (< y (array-dimension m 1))))

(defun look-toward (x y dir)
  (list (+ x (first dir))
        (+ y (second dir))))


(defparameter +directions+
  '((-1 -1) (+0 -1) (+1 -1)
    (-1 +0)         (+1 +0)
    (-1 +1) (+0 +1) (+1 +1)))

(defun neighbors-of (m x y)
  (mapcar #'(lambda (xy) (apply #'aref m xy))
          (remove-if-not #'(lambda (xy) (apply #'valid-coordinate m xy))
                         (mapcar #'(lambda (dir) (look-toward x y dir)) +directions+))))

;; waiting-area utilities
(defconstant +nil+ nil)
(defconstant +empty+ #\L)
(defconstant +occupied+ #\#)
(defconstant +floor+ #\.)

(defun visible-seat-in-direction-p (m x y dir)
  (destructuring-bind (new-x new-y) (look-toward x y dir)
    (cond ((not (valid-coordinate m new-x new-y))
           +nil+)
          ((char= +floor+ (aref m new-x new-y))
           (visible-seat-in-direction-p m new-x new-y dir))
          (t (aref m new-x new-y)))))

(defun visible-seats-from (m x y)
  (mapcar #'(lambda (dir) (visible-seat-in-direction-p m x y dir)) +directions+))

(defun count-seats (seat waiting-area)
  (let ((count 0))
    (aoc:map-array #'(lambda (m w h) (when (char= seat (aref m h w))
                                  (incf count)))
                  waiting-area)
    count))

(defun valid-waiting-area-p (waiting-area)
  (zerop (count-seats +nil+ waiting-area)))



;;
;; Rules
;;
(defun do-nothing-rule (m x y) (aref m x y))
(defun empty-space-rule (m x y)
  (if (zerop (count +occupied+ (neighbors-of m x y))) +occupied+ +empty+))
(defun occupied-space-rule (m x y)
  (if (<= 4 (count +occupied+ (neighbors-of m x y))) +empty+ +occupied+))

(defparameter +neighbor-rules+ (list
                                (cons +floor+  #'do-nothing-rule)
                                (cons +empty+ #'empty-space-rule)
                                (cons +occupied+  #'occupied-space-rule)))

(defun empty-visibility-rule (m x y)
  (if (zerop (count +occupied+ (visible-seats-from m x y))) +occupied+ +empty+))

(defun occupied-visibility-rule (m x y)
  (if (<= 5 (count +occupied+ (visible-seats-from m x y))) +empty+ +occupied+))

(defparameter +visibility-rules+ (list (cons +floor+ #'do-nothing-rule)
                                       (cons +empty+ #'empty-visibility-rule)
                                       (cons +occupied+ #'occupied-visibility-rule)))



(defun apply-rules-to-cell (rules waiting-area x y)
  (funcall (cdr (assoc (aref waiting-area x y) rules)) waiting-area x y))

(defun apply-rules (waiting-area rules)
  (aoc:map-array #'(lambda (m w h)
                     (apply-rules-to-cell rules m h w))
                 waiting-area
                 ;; (partial #'apply-rules-to-cell rules)
                 ))

(defun apply-rules-until-fixpoint (waiting-area rules)
  (do* ((old-area waiting-area)
        (new-area (apply-rules old-area rules)
                  (apply-rules old-area rules))
        (count 1 (incf count)))
       ((equalp old-area new-area) (values old-area count))
    (setf old-area (apply-rules new-area rules))))


;;
;; Inputs
;;

(defparameter +input+ (read-data (today-data-pathname)
                                 :post-process #'aoc:lists->2d-array))
(defparameter +example+ (read-data (today-data-pathname "example")
                                   :post-process #'aoc:lists->2d-array))


;; part I
(5am:def-test part1 (:suite :aoc-2020-11)
  (5am:is (= 37 (count-seats +occupied+ (apply-rules-until-fixpoint +example+ +neighbor-rules+))))
  (5am:is (= 2289 (count-seats +occupied+ (apply-rules-until-fixpoint +input+ +neighbor-rules+)))))

;; part II
(5am:def-test part2 (:suite :aoc-2020-11)
  (5am:is (= 26 (count-seats +occupied+ (apply-rules-until-fixpoint +example+ +visibility-rules+))))
  (5am:is (= 2059 (count-seats +occupied+ (apply-rules-until-fixpoint +input+ +visibility-rules+)))))
