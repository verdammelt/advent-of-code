(defpackage #:aoc-2021-08
  (:use :cl))

(in-package #:aoc-2021-08)

(aoc:def-today-suite*)

;;   0:      1:      2:      3:      4:
;;  aaaa    ....    aaaa    aaaa    ....
;; b    c  .    c  .    c  .    c  b    c
;; b    c  .    c  .    c  .    c  b    c
;;  ....    ....    dddd    dddd    dddd
;; e    f  .    f  e    .  .    f  .    f
;; e    f  .    f  e    .  .    f  .    f
;;  gggg    ....    gggg    gggg    ....

;;   5:      6:      7:      8:      9:
;;  aaaa    aaaa    aaaa    aaaa    aaaa
;; b    .  b    .  .    c  b    c  b    c
;; b    .  b    .  .    c  b    c  b    c
;;  dddd    dddd    ....    dddd    dddd
;; .    f  e    f  .    f  e    f  .    f
;; .    f  e    f  .    f  e    f  .    f
;;  gggg    gggg    ....    gggg    gggg

;; number of segments used:
;; 0 - 6
;; 1 - 2
;; 2 - 5
;; 3 - 5
;; 4 - 4
;; 5 - 5
;; 6 - 6
;; 7 - 3
;; 8 - 7
;; 9 - 6

(defun make-input-entry (patterns values)
  (list patterns values))
(defun signal-paterns (input-entry) (first input-entry))
(defun output-values (input-entry) (second input-entry))

(defun parse-line (line)
  (destructuring-bind (patterns values)
      (mapcar #'(lambda (x) (aoc:split-string-on-char #\Space x))
              (aoc:split-string-on-char #\| line))
    (make-input-entry patterns values)))

(defun read-data (file)
  (aoc:read-data file :line-parser #'parse-line))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +longer+
  (read-data (aoc:today-data-pathname "longer")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defparameter +digit-num-segments+
  '((0 . 6)
    (1 . 2)
    (2 . 5)
    (3 . 5)
    (4 . 4)
    (5 . 5)
    (6 . 6)
    (7 . 3)
    (8 . 7)
    (9 . 6)))

(defun flatten (lists)
  (apply #'append lists))

;; we only care about 1, 4, 7, 8 which have unique counts of segments. Since we
;; only care about how many of these there are we can just count the items with
;; the correct lengths!
(defun part1 (input)
  (let ((all-outputs (flatten (mapcar #'output-values input)))
        (important-digits '(1 4 7 8)))
    (count-if #'(lambda (d) (member (car (rassoc (length d) +digit-num-segments+))
                               important-digits))
              all-outputs)))

(5am:def-test part1 (:suite :aoc-2021-08)
  (5am:is (= 26 (part1 +longer+)))
  (5am:is (= 525 (part1 +input+))))

;; (defun part2 (input) :to-be-implemented)

;; (5am:def-test part2 (:suite :aoc-2021-08)
;;   (5am:is (= :answer (part2 +example+)))
;;   (5am:is (= :answer (part2 +part1+))))
