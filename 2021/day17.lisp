(defpackage #:aoc-2021-17
  (:use :cl))

(in-package #:aoc-2021-17)

(aoc:def-today-suite*)

(defun parse-target-coords (s)
  (let ((coords
          (remove nil (mapcar #'(lambda (s) (parse-integer s :junk-allowed t))
                              (aoc:split-string-on-chars '(#\Space #\= #\. #\,) s)))))
    (list (subseq coords 0 2)
          (subseq coords 2))))

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'parse-target-coords
                 :post-process #'first))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun make-coord (x y) (cons x y))
(defun coord-x (c) (car c))
(defun coord-y (c) (cdr c))

(defun coord-+ (c1 c2)
  (make-coord (+ (coord-x c1) (coord-x c2))
              (+ (coord-y c1) (coord-y c2))))

(defun make-probe (location velocity) (cons location velocity))
(defun probe-location (p) (car p))
(defun probe-velocity (p) (cdr p))

(defun apply-drag (x)
  (cond ((zerop x) 0)
        ((plusp x) (1- x))
        ((minusp x) (1+ x))))

(defun apply-gravity (y)
  (1- y))

(defun step-probe (p)
  (make-probe
   (coord-+ (probe-location p) (probe-velocity p))
   (make-coord (apply-drag (coord-x (probe-velocity p)))
               (apply-gravity (coord-y (probe-velocity p))))))

(defun past-target-p (location target)
  (or (> (coord-x location) (apply #'max (first target)))
      (< (coord-y location) (apply #'min (second target)))))

(defun in-target-p (location target)
  (and (<= (first (first target))
           (coord-x location)
           (second (first target)))
       (<= (first (second target))
           (coord-y location)
           (second (second target)))))

(defun will-land-in-target-p (probe target)
  (cond ((past-target-p (probe-location probe) target) nil)
        ((in-target-p (probe-location probe) target) t)
        (t (will-land-in-target-p (step-probe probe) target))))

(defun gauss-sum (n) (/ (* n (1+ n)) 2))

;; TODO: Can the brute force methods in part1 and part2 be made better? perhaps
;; analyzing the x,y boundaries for velocity instead of 0-1000... I feel that X
;; can be bounded by the right edge of the target area - but how to upper bind
;; y? (here trial and error shows that 105 is ok for the inputs...) Is it
;; (ABS min-y)?
(defun find-all-target-velocities (target)
  "!!!BRUTE FORCE!!!"
  (flet ((start-probe (vel-x vel-y) (make-probe (make-coord 0 0)
                                                (make-coord vel-x vel-y))))
    (loop for x from 0 to (second (first target))
          append (loop for y from (first (second target)) to 105
                       for probe = (start-probe x y) then (start-probe x y)
                       when (will-land-in-target-p probe target)
                         collect probe)
            into probes
          finally (return (mapcar #'probe-velocity probes)))))

(defun part1 (input)
  (let ((velocities (find-all-target-velocities input)))
    (gauss-sum (coord-y (first (sort velocities #'> :key #'coord-y))))))

(5am:def-test part1 (:suite :aoc-2021-17)
  (5am:is (= 45 (part1 +example+)))
  (5am:is (= 5460 (part1 +input+))))

(defun part2 (input)
  (length (find-all-target-velocities input)))

(5am:def-test part2 (:suite :aoc-2021-17)
  (5am:is (= 112 (part2 +example+)))
  (5am:is (= 3618 (part2 +input+))))
