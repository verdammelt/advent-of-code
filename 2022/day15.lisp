(defpackage #:aoc-2022-15
  (:use :cl))

(in-package #:aoc-2022-15)

(aoc:def-today-suite*)

(declaim (inline make-point))
(defun make-point (x y)
  (list x y))

(defun manhattan-distance (p1 p2)
  (let ((x1 (first p1)) (y1 (second p1))
        (x2 (first p2)) (y2 (second p2)))
    (+ (abs (- x1 x2))
       (abs (- y1 y2)))))

(defun parse-integer-with-junk (str) (parse-integer str :junk-allowed t))

(defun parse-line (str)
  (let ((nums (mapcar #'parse-integer-with-junk (rest (aoc:split-string-on-char #\= str)))))
    (list (make-point (first nums) (second nums))
          (make-point (third nums) (fourth nums)))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-line))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

;;;
;;; Intervals idea from reddit, code inspiration from:
;;; <https://github.com/blake-watkins/advent-of-code-2022/blob/main/day15.lisp>
;;;
;; TODO: extract intervals implementation to library?

(defun interval-for-sensor-beacon-in-row (row sensor beacon)
  (let ((covered-range (manhattan-distance sensor beacon)))
    (destructuring-bind (x y) sensor
      (let ((diff-for-row (abs (- y row))))
        (if (<= diff-for-row covered-range)
            (list (+ (- x covered-range) diff-for-row)
                  (- (+ x covered-range) diff-for-row))
            nil)))))

(defun normalize-intervals (intervals)
  "Take list of INTERVALS (in any order and/or overlapping). Return list of
disjoint intervals of the biggest possible sizes."
  (labels ((normalize (current intervals)
             (cond ((null current) nil)
                   ((null intervals) (list current))
                   (t (destructuring-bind (next . rest) intervals
                        (if (<= (first next) (1+ (second current)))
                            (normalize (list (first current)
                                             (max (second current) (second next)))
                                       rest)
                            (cons current
                                  (normalize (first rest) (rest rest)))))))))

    (let ((intervals (sort intervals #'< :key #'first)))
      (normalize (first intervals) (rest intervals)))))

(defun intervals-in-row (sensor-beacons row)
  (normalize-intervals
   (remove nil
           (mapcar
            #'(lambda (sb) (apply #'interval-for-sensor-beacon-in-row row sb))
            sensor-beacons))))


(defun find-interval-containing (intervals value)
  (find-if #'(lambda (int) (<= (first int) value (second int))) intervals))

(defun num-covered-beacons (sensor-beacons intervals row)
  (length (remove-duplicates
           (do ((sbs sensor-beacons (rest sbs))
                (beacon-xs nil))
               ((null sbs) beacon-xs)
             (destructuring-bind (sensor (x y)) (first sbs)
               (declare (ignore sensor))
               (when (and (= y row) (find-interval-containing intervals x))
                 (push x beacon-xs)))))))

(defun intervals-size (intervals)
  "Assumes all intervals are already normalized"
  (reduce #'+ intervals :key #'(lambda (int) (1+ (- (second int) (first int))))))

(defun part1 (input row)
  (let* ((intervals (intervals-in-row input row))
         (num-beacons (num-covered-beacons input intervals row)))
    (- (intervals-size intervals) num-beacons)))

(5am:def-test part1 (:suite :aoc-2022-15)
  (5am:is (= 26 (part1 +example+ 10)))
  (5am:is (= 4582667 (part1 +input+ 2000000))))

(defun part2 (input upper-limit)
  (destructuring-bind (x y)
      (block gap
        (do ((y 0 (1+ y)))
            ((> y upper-limit))
          (do ((x 0))
              ((> x upper-limit))
            (let ((containing (find-interval-containing (intervals-in-row input y) x)))
              (if (not containing) (return-from gap (list x y))
                  (setf x (1+ (second containing))))))
          ))
    (+ (* 4000000 x) y)))

(5am:def-test part2 (:suite :aoc-2022-15)
  (5am:is (= 56000011 (part2 +example+ 20)))
  (5am:is (= 10961118625406 (part2 +input+ 4000000))))
