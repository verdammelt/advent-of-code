(defpackage #:aoc-2021-21
  (:use :cl))

(in-package #:aoc-2021-21)

(aoc:def-today-suite*)

(defun parse-starting-positions (str)
  (parse-integer (nth 4 (aoc:split-string-on-char #\Space str))))

(defun read-data (file)
  (aoc:read-data file :line-parser #'parse-starting-positions))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defvar *number-of-die-rolls* 0)

(defun deterministic-100-sider ()
  (let ((value 0))
    (lambda () (let ((new-value (1+ value)))
            (setf value
                  (if (> new-value 100) 1 new-value))))))

(defun three-roll-die (x y z)
  (let ((values (list x y z)))
    (lambda () (pop values))))

(defun roll (die) (incf *number-of-die-rolls*) (funcall die) )

(defun take-turn (die position)
  (let ((new-position
          (apply #'+ position (loop for i below 3 collect (roll die)))))
    (if (> new-position 10)
        (if (zerop (mod new-position 10)) 10 (mod new-position 10))
        new-position)))

(defun play-game (starting-positions die &optional max-turns)
  (setq *number-of-die-rolls* 0)
  (let ((player1-score 0)
        (player1-position (first starting-positions))
        (player2-score 0)
        (player2-position (second starting-positions)))
    (loop for turn from 1
          until (or (> turn (or max-turns (1+ turn)))
                 (some #'(lambda (n) (>= n 1000)) (list player1-score player2-score)))
          do (progn
               (if (oddp turn)
                   (setf player1-position (take-turn die player1-position)
                         player1-score (+ player1-score player1-position))
                   (setf player2-position (take-turn die player2-position)
                         player2-score (+ player2-score player2-position)))
               ;; (print (list (if (oddp turn) 'player1 'player2)
               ;;              (list player1-score player1-position)
               ;;              (list player2-score player2-position)))
               )
          finally (return (* *number-of-die-rolls* (min player1-score player2-score))))))

(defun part1 (input)
  (play-game input (deterministic-100-sider)))

(5am:def-test part1 (:suite :aoc-2021-21)
  (5am:is (= 739785 (part1 +example+)))
  (5am:is (= 925605 (part1 +input+))))

(defun part2 (input) (declare (ignore input)) 0)

;; TODO: complete 2021-21.2
(5am:def-test part2 (:suite :aoc-2021-21)
  (5am:skip ":aoc-2021-21.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
