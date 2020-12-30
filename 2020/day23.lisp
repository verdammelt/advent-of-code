(defpackage #:aoc-2020-23
  (:use :cl))

(in-package #:aoc-2020-23)

(aoc:def-today-suite*)

(defparameter +example+ "389125467")
(defparameter +input+ "538914762")

(defun parse-to-cup-labels (str)
  (map 'list #'digit-char-p str))

(defstruct cup-game circle current max min)

(defun start-cup-game (cups)
  (make-cup-game :circle cups :current 0
                 :min (reduce #'min cups)
                 :max (reduce #'max cups)))

(defun play-game (game)
  (let ((three-cups (subseq (cup-game-circle game)
                            (+ current 1)
                            (+ current 3)))
        (remaining (append (subseq (cup-game-circle game) 0 currrent)))))
)

(defun part1 (input turns)
  (let* ((cups (parse-to-cup-labels input))
         (game (start-cup-game cups)))
    (dotimes (i turns)
      (play-game game))))

(5am:def-test part1 (:suite :aoc-2020-23)
  ;; (5am:is (string= "92658374" (part1 +example+ 10)))
  ;; (5am:is (string= "67384529" (part1 +example+ 100)))
  ;; (5am:is (string= "" (part1 +input+ 100)))
  )
