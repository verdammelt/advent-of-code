(defpackage #:aoc-2023-18
  (:use :cl))

(in-package #:aoc-2023-18)

(aoc:def-today-suite*)

(defun parse-dig-command (s)
  (let ((parts (aoc:split-string-on-char #\Space s)))
    (list (aoc:keywordize (first parts))
          (parse-integer (second parts))
          (third parts))))

(defun command-direction (cmd) (first cmd))
(defun command-distance (cmd) (second cmd))
(defun command-color (cmd) (third cmd))

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'parse-dig-command))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun direction-delta (dir)
  (ecase dir
    (:u #C (-1 0))
    (:d #C (1 0))
    (:l #C (0 -1))
    (:r #C (0 1))))


(defun dig (cmds)
  (loop for start = #C (0 0) then start
        for c in cmds
        append (loop for i upto (command-distance c)
                     collect (+ start (* i (direction-delta (command-direction c)))))
          into edges
        do (setf start (first (last edges)))
        finally (return (remove-duplicates edges))))

(defun trench-bounds (trench)
  (loop for h in trench
        for x = (realpart h) then (realpart h)
        and y = (imagpart h) then (imagpart h)
        minimizing x into min-x
        maximizing x into max-x
        minimizing y into min-y
        maximizing y into max-y
        finally (return (list (list min-x max-x)
                              (list min-y max-y)))))

(defun shoelace (points)
  (loop for (p1 p2) on (append points (list (first points)))
        while (and p1 p2)
        for x1 = (realpart p1) and x2 = (realpart p2)
        for y1 = (imagpart p1) and y2 = (imagpart p2)
        sum (* x1 y2) into sum1
        sum (* x2 y1) into sum2
        finally (return (abs (/ (- sum1 sum2) 2)))))

(defun part1 (input)
  "Shoelace gives us the area inside the trench as if each coordinate of trench
cube was the *center* of that cube - so we need to add in a half cube for every
piece of trench."
  (let ((trench (dig input)))
    (+ (shoelace trench) ;; area *inside* the trench
       (/ (length trench) 2) ;; area of the trench itself
       1) ;; just cause! :)
    ))

(5am:def-test part1 (:suite :aoc-2023-18)
  (5am:is (= 38 (length (dig +example+))))
  (5am:is (= 62 (part1 +example+)))
  (5am:is (= 62500 (part1 +input+))))

(defun parse-true-command (cmd)
  (let* ((color (command-color cmd))
         (hex-str (subseq color 2 (1- (length color)))))
    (list (ecase (char hex-str 5)
            (#\0 :r)
            (#\1 :d)
            (#\2 :l)
            (#\3 :u))
          (parse-integer (subseq hex-str 0 5) :radix 16)
          hex-str)))

;; this is the right idea - but actually computing the entire trench and then
;; shoelacing it will not work!
;;
;; need to try counting as we go digging the ditch - we only need one hole and
;; the next and the sum as we go...
(defun part2 (input)
  (let* ((fixed-input (mapcar #'parse-true-command input))
         (trench (dig fixed-input)))
    (+ (shoelace trench)
       (/ (length trench) 2)
       1)))


(5am:def-test part2 (:suite :aoc-2023-18)
  (5am:skip ":aoc-2023-18.2 not implemented")
  ;; (5am:is (= 952408144115 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
