(defpackage #:aoc-2020-22
  (:use :cl #:aoc-2020/utils #:aoc))

(in-package #:aoc-2020-22)

(5am:def-suite :aoc-2020-22 :in :aoc-2020)

(defun parse-deck (lines)
  (mapcar #'parse-integer (cdr lines)))

(defun read-data-file (&optional label)
  (mapcar #'parse-deck
          (read-data (today-data label) :post-process #'split-on-empty-line)))

(defparameter +example+ (read-data-file "example"))
(defparameter +input+ (read-data-file))

(defun play-combat (player1 player2)
  (let ((card1 (car player1))
        (deck1 (cdr player1))
        (card2 (car player2))
        (deck2 (cdr player2)))
    (cond ((null card1) (values :player2 player2))
          ((null card2) (values :player1 player1))
          ((> card1 card2) (play-combat (append deck1 (list card1 card2)) deck2))
          (t (play-combat deck1 (append deck2 (list card2 card1)))))))

(defun score-deck (deck)
  (labels ((%score (cards idx score)
             (cond ((null cards) score)
                   (t (%score (cdr cards) (1+ idx) (+ score (* idx (car cards))))))))
    (%score (reverse deck) 1 0)))

(defun part1 (input)
  (multiple-value-bind (winner winning-deck) (apply #'play-combat input)
    (values winner (score-deck winning-deck))))

(5am:def-test part1 (:suite :aoc-2020-22)
  (5am:is (equal (values :player2 306) (part1 +example+)))
  (5am:is (equal (values :player1 32472) (part1 +input+))))

(defparameter *debug-p* nil)
(defun debug-fmt (fmt &rest args)
  (when *debug-p*
    (fresh-line *debug-io*)
    (apply #'format *debug-io* fmt args)))

(defun play-recursive-combat (player1 player2)
  (debug-fmt "PLAY-RECURSIVE-COMBAT ~S ~S" player1 player2)
  (let ((all-previous-decks (list)))
    (labels ((remember-decks (deck1 deck2) (push (list deck1 deck2) all-previous-decks))
             (seen-decks-p (deck1 deck2) (member (list deck1 deck2) all-previous-decks :test #'equalp))
             (%play (player1 player2)
               (debug-fmt "%PLAY ~S ~S" player1 player2)
               (cond ((seen-decks-p player1 player2)
                      (debug-fmt "SEEN-DECKS ~S ~S -- PLAYER1 wins" player1 player2)
                      (values :player1 player1))
                     ((null player1) (values :player2 player2))
                     ((null player2) (values :player1 player1))
                     ((and (<= (car player1) (length (cdr player1)))
                           (<= (car player2) (length (cdr player2))))
                      (debug-fmt "RECURSING...")
                      (remember-decks player1 player2)
                      (case (play-recursive-combat (subseq player1 1 (1+ (car player1)))
                                                   (subseq player2 1 (1+ (car player2))))
                        (:player1 (%play (append (cdr player1)
                                                 (list (car player1)
                                                       (car player2)))
                                         (cdr player2)))
                        (:player2 (%play (cdr player1)
                                         (append (cdr player2)
                                                 (list (car player2)
                                                       (car player1)))))))

                     (t (debug-fmt "NORMAL CASE...")
                        (remember-decks player1 player2)
                        (if (> (car player1) (car player2))
                            (%play (append (cdr player1)
                                           (list (car player1) (car player2)))
                                   (cdr player2))
                            (%play (cdr player1)
                                   (append (cdr player2)
                                           (list (car player2) (car player1)))))))))
      (%play player1 player2))))

(defun part2 (input)
  (multiple-value-bind (winner winning-deck)
      (apply #'play-recursive-combat input)
    (values winner (score-deck winning-deck))))

(5am:def-test part2 (:suite :aoc-2020-22)
  (5am:is (equal (values :player2 291) (part2 +example+)))
  (5am:is (equal (values :player1 36463) (part2 +input+))))
