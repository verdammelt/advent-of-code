(defpackage #:aoc-2024-22
  (:use :cl))

(in-package #:aoc-2024-22)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :line-parser #'parse-integer))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun mix (secret number)
  (logxor secret number))

(defun prune (number)
  (mod number 16777216))

(defun next-secret (secret)
  (flet ((mix-and-prune (secret number) (prune (mix secret number))))

    (let* ((step1 (mix-and-prune secret (* secret 64)))
           (step2 (mix-and-prune step1 (floor step1 32)))
           (step3 (mix-and-prune step2 (* step2 2048))))
      step3)))

(defun nth-next-secret (secret nth-secret)
  (dotimes (n nth-secret secret)
    (setf secret (next-secret secret))))

(defun part1 (input)
  (aoc:sum (mapcar #'(lambda (secret) (nth-next-secret secret 2000)) input)))

(5am:def-test part1 (:suite :aoc-2024-22)
  (5am:is (equal '(123 15887950 16495136 527345 704524 1553684 12683156
                   11100544 12249484 7753432 5908254)
                 (loop for x upto 10
                       for secret = 123 then (next-secret secret)
                       collect secret)))
  (5am:is (= 37327623 (part1 +example+)))
  (5am:is (= 13234715490 (part1 +input+))))

(defun n-secrets (initial-secret n)
  (loop for x upto n
        for secret = initial-secret then (next-secret secret)
        collect secret))

(defun ones-digit (n) (rem n 10))

(defun diffs (nums) (mapcar #'- (cdr nums) nums))

(defun gather-4-diff-sequences-for-buyer
    (buyer &optional (four-diff-sequences (make-hash-table :test #'equalp)))
  (let* ((secrets (n-secrets buyer 2001))
         (prices (mapcar #'ones-digit secrets))
         (diffs (diffs prices))
         (seen (make-hash-table :test #'equalp)))
    (maplist #'(lambda (price-list diff-list)
                 (when (>= (length price-list) 4)
                   (let ((4-prices (subseq price-list 0 4))
                         (4-diffs (subseq diff-list 0 4)))
                     (unless (gethash 4-diffs seen)
                       (setf (gethash 4-diffs seen) t)
                       (push (first (last 4-prices))
                             (gethash 4-diffs four-diff-sequences (list)))))))
             (cdr prices)
             diffs))
  four-diff-sequences)

(defun find-max-banana-sequence (four-diff-sequences)
  (let ((max-bananas 0)
        (max-bananas-sequence nil))
    (maphash #'(lambda (seq bananas)
                 (when (> (aoc:sum bananas) max-bananas)
                   (setq max-bananas (aoc:sum bananas)
                         max-bananas-sequence seq)))
             four-diff-sequences)
    (values max-bananas max-bananas-sequence)))

(defparameter +part2-example+ (list 1 2 3 2024))

(defun part2 (input)
  (let ((four-diff-sequences (make-hash-table :test #'equalp)))
    (dolist (buyer input four-diff-sequences)
      (gather-4-diff-sequences-for-buyer buyer four-diff-sequences))
    (find-max-banana-sequence four-diff-sequences)))

(5am:def-test part2 (:suite :aoc-2024-22)
  (5am:is (equal '(7) (gethash '(-2 1 -1 3)
                               (gather-4-diff-sequences-for-buyer 1))))
  (5am:is (equal '(7) (gethash '(-2 1 -1 3)
                               (gather-4-diff-sequences-for-buyer 2))))
  (5am:is (equal nil (gethash '(-2 1 -1 3)
                              (gather-4-diff-sequences-for-buyer 3))))
  (5am:is (equal '(9) (gethash '(-2 1 -1 3)
                               (gather-4-diff-sequences-for-buyer 2024))))
  (5am:is (= 23 (part2 +part2-example+)))
  (5am:is (equal '(-2 1 -1 3) (nth-value 1 (part2 +part2-example+))))
  (5am:is (= 1490 (part2 +input+))))
