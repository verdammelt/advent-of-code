(defpackage #:aoc-2023-07
  (:use :cl)
  (:shadow :type))

(in-package #:aoc-2023-07)

(aoc:def-today-suite*)

(defun card-counts (cards)
  "Returns a hash-table of CARD->COUNT for each CARD in CARDS."
  (reduce #'(lambda (counts card) (incf (gethash card counts 0)) counts)
          cards
          :initial-value (make-hash-table))  )

(defun categorize-hand (raw-hand)
  (let* ((card-counts (card-counts raw-hand))
         (num-unique-keys (length (alexandria:hash-table-keys card-counts)))
         (max-same-cards (apply #'max (alexandria:hash-table-values card-counts))))
    (cond ((= num-unique-keys 1) :five-of-a-kind)
          ((and (= num-unique-keys 2) (= max-same-cards 4)) :four-of-a-kind)
          ((= num-unique-keys 2) :full-house)
          ((and (= num-unique-keys 3) (= max-same-cards 3)) :three-of-a-kind)
          ((= num-unique-keys 3) :two-pair)
          ((= num-unique-keys 4) :one-pair)
          (t :high-card))))

(defun make-hand (cards type bid) (list cards type bid))

(defun cards (hand) (first hand))
(defun type (hand) (second hand))
(defun bid (hand) (third hand))

(defun parse-hand (str)
  (destructuring-bind (cards bid)
      (aoc:split-string-on-char #\Space str)
    (let ((raw-hand (coerce cards 'list)))
      (make-hand raw-hand (categorize-hand raw-hand) (parse-integer bid)))))

(defun type-equal-p (type1 type2) (eq type1 type2))
(defun type-less-p (type1 type2)
  (let ((type-order '(:high-card :one-pair :two-pair :three-of-a-kind :full-house
                      :four-of-a-kind :five-of-a-kind)))
    (< (position type1 type-order)
       (position type2 type-order))))

(defun card-equal-p (card1 card2) (eq card1 card2))

(defparameter *card-value-order*
  '(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K #\A))
(defparameter *card-value-order-with-jokers*
  '(#\J #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\Q #\K #\A))

(defun card-less-p (card1 card2)
  (< (position card1 *card-value-order*)
     (position card2 *card-value-order*)))

(defun cards-less-p (cards1 cards2)
  (cond ((null cards1) nil)
        ((not (card-equal-p (first cards1) (first cards2)))
         (card-less-p (first cards1) (first cards2)))
        (t (cards-less-p (rest cards1) (rest cards2)))))

(defun hand-less-p (hand1 hand2)
  (cond ((type-equal-p (type hand1) (type hand2))
         (cards-less-p (cards hand1) (cards hand2)))
        (t (type-less-p (type hand1) (type hand2)))))

(defun read-data (file) (aoc:read-data file  :line-parser #'parse-hand))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun order-hands (hands)
  (sort (copy-seq hands) #'hand-less-p))

(defun total-winnings (ordered-hands)
  (loop for h in ordered-hands
        for rank from 1
        sum (* (bid h) rank)))

(defun part1 (input) (total-winnings (order-hands input)))

(5am:def-test part1 (:suite :aoc-2023-07)
  (5am:is (= 6440 (part1 +example+)))
  (5am:is (= 251029473 (part1 +input+))))

(defun improve-hand-with-jokers (hand)
  (if (or (type-equal-p :five-of-a-kind (type hand))
          (not (member #\J (cards hand))))
      hand ;; cannot be improved.
      (let* ((cards (cards hand))
             (card-counts (card-counts cards))
             (sorted-counts (sort (alexandria:hash-table-alist card-counts) #'> :key #'cdr))
             (max-card (car (find-if-not #'(lambda (c) (char= c #\J)) sorted-counts :key #'car))))
        (make-hand cards (categorize-hand (substitute max-card #\J (copy-seq cards))) (bid hand)))))

(defun part2 (input)
  (let ((*card-value-order* *card-value-order-with-jokers*))
    (total-winnings (order-hands (mapcar #'improve-hand-with-jokers input)))))

(5am:def-test part2 (:suite :aoc-2023-07)
  (5am:is (= 5905 (part2 +example+)))
  (5am:is (= 251003917 (part2 +input+))))
