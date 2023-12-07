(defpackage #:aoc-2023-04
  (:use :cl))

(in-package #:aoc-2023-04)

(aoc:def-today-suite*)

(defun card-number (card) (first card))
(defun card-winning-numbers (card) (second card))
(defun card-your-numbers (card) (third card))

(defun parse-scratch-card (str)
  (let ((raw-card (aoc:split-string-on-chars '(#\: #\|) str)))
    (list (parse-integer (subseq (first raw-card) (length "card")))
          (aoc:string-of-numbers->list-of-numbers (second raw-card))
          (aoc:string-of-numbers->list-of-numbers (third raw-card)))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-scratch-card))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun number-of-wins (card)
  (length (intersection (card-winning-numbers card) (card-your-numbers card))))

(defun points-for-card (card)
  (if (< 0 (number-of-wins card))
      (expt 2 (1- (number-of-wins card)))
      0))

(defun part1 (input)
  (aoc:sum (mapcar #'points-for-card input)))

(5am:def-test part1 (:suite :aoc-2023-04)
  (5am:is (= 13 (part1 +example+)))
  (5am:is (= 20107 (part1 +input+))))

(defun card-and-wins (cards)
  (loop for card in cards
        for x = 1 then (1+ x)
        collect (list x (number-of-wins card))))

(defun total-up-cards (card-wins)
  "CARD-WINS is a list of (CARD-NUM NUM-WINS).
Create a list of counts for cards - initially all 1. For each card N with wins W
increment count of each card N+1..N+w by the count of card N."
  (let ((num-cards (make-list (length card-wins) :initial-element 1)))
    (loop for (num wins) in card-wins
          do (dotimes (i wins)
               (incf (nth (+ num i) num-cards) (nth (1- num) num-cards))))
    (aoc:sum num-cards)))

(defun part2 (input)
  (total-up-cards (card-and-wins input)))

(5am:def-test part2 (:suite :aoc-2023-04)
  (5am:is (= 30 (part2 +example+)))
  (5am:is (= 8172507 (part2 +input+))))
