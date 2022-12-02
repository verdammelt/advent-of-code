(defpackage #:aoc-2022-02
  (:use :cl))

(in-package #:aoc-2022-02)

(aoc:def-today-suite*)

(defun split-on-space (s) (aoc:split-string-on-char #\Space s))

(defun read-data (file) (aoc:read-data file :line-parser #'split-on-space))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defvar *winning-moves* '((:rock . :scissor)
                          (:paper . :rock)
                          (:scissor . :paper))
  "The moves that result in a win for the first player")

(defun result (moves)
  (destructuring-bind (us . them) moves
    (cond ((eq us them) :draw)
          ((member moves *winning-moves* :test #'equal) :win)
          (t :lose))))

(5am:def-test result ()
  (5am:is (eq :draw (result '(:rock . :rock))))
  (5am:is (eq :draw (result '(:paper . :paper))))
  (5am:is (eq :draw (result '(:scissor . :scissor))))
  (5am:is (eq :win (result '(:rock . :scissor))))
  (5am:is (eq :win (result '(:paper . :rock))))
  (5am:is (eq :win (result '(:scissor . :paper))))
  (5am:is (eq :lose (result '(:scissor . :rock))))
  (5am:is (eq :lose (result '(:rock . :paper))))
  (5am:is (eq :lose (result '(:paper . :scissor)))))

(defun result-score (result)
  (case result
    (:win 6)
    (:draw 3)
    (:lose 0)))

(defun move-score (move)
  (case move
    (:rock 1)
    (:paper 2)
    (:scissor 3)))

(defun my-score (moves)
  (let ((result-score (result-score (result (cons (cdr moves) (car moves)))))
        (move-score (move-score (cdr moves))))
    (+ result-score move-score)))

(5am:def-test scoring (:suite :aoc-2022-02)
  (5am:is (= 8 (my-score '(:rock . :paper))))
  (5am:is (= 1 (my-score '(:paper . :rock))))
  (5am:is (= 6 (my-score '(:scissor . :scissor)))))

(defun parse-move (move-string)
  "Simple mapping of ABC/XYZ => rock/paper/scissor"
  (cdr (assoc move-string '(("A" . :rock) ("B" . :paper) ("C" . :scissor)
                            ("X" . :rock) ("Y" . :paper) ("Z" . :scissor))
              :test #'string=)))

(defun parse-both-moves-line (moves)
  "Parse both items in MOVES as if they were rock/paper/scissor"
  (cons (parse-move (first moves))
        (parse-move (second moves))))

(defun part1 (input)
  (aoc:sum (mapcar #'my-score (mapcar #'parse-both-moves-line input))))

(5am:def-test part1 (:suite :aoc-2022-02)
  (5am:is (= 10310 (part1 +input+))))

(defun parse-result (result-str)
  (cdr (assoc result-str '(("X" . :lose) ("Y" . :draw) ("Z" . :win))
              :test #'string=)))

(defun guess-move (their-move result)
  (case result
    (:draw their-move)
    (:lose (cdr (find their-move *winning-moves* :key #'car)))
    (:win (car (find their-move *winning-moves* :key #'cdr)))))

(defun parse-guide-line (moves)
  (let* ((their-move (parse-move (first moves)))
         (result (parse-result (second moves)))
         (our-move (guess-move their-move result)))
    (cons their-move our-move)))

(5am:def-test parsing-guide-lines (:suite :aoc-2022-02)
  (5am:is (equal '(:rock . :rock) (parse-guide-line '("A" "Y"))))
  (5am:is (equal '(:paper . :rock) (parse-guide-line '("B" "X"))))
  (5am:is (equal '(:scissor . :rock) (parse-guide-line '("C" "Z")))))

(defun part2 (input)
  (aoc:sum (mapcar #'my-score (mapcar #'parse-guide-line input))))

(5am:def-test part2 (:suite :aoc-2022-02)
  (5am:is (= 14859 (part2 +input+))))
