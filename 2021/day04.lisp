(defpackage #:aoc-2021-04
  (:use :cl))

(in-package #:aoc-2021-04)

(aoc:def-today-suite*)

(defstruct bingo-subsytem numbers boards)

(defstruct bingo-board columns rows)

(defun mark-number (board number)
  (let ((columns (bingo-board-columns board))
        (rows (bingo-board-rows board)))
    (make-bingo-board :columns (mapcar #'(lambda (c) (substitute nil number c)) columns)
                      :rows (mapcar #'(lambda (r) (substitute nil number r)) rows))))

(defun winning-board-p (board)
  (let ((columns (bingo-board-columns board))
        (rows (bingo-board-rows board)))
    (or (find-if #'(lambda (c) (every #'null c)) columns)
        (find-if #'(lambda (r) (every #'null r)) rows))))

(defun unmarked-numbers (board)
  (let ((columns (bingo-board-columns board)))
    (remove nil (apply #'concatenate 'list columns))))

(defun transpose (columns)
  (loop for i below (length (first columns))
        collect (mapcar #'(lambda (col) (nth i col)) columns)))

(defun parse-board (raw-board-data)
  (let* ((columns (mapcar #'(lambda (s) (aoc:string-of-numbers->list-of-numbers s #\Space)) raw-board-data))
         (rows (transpose columns)))
    (make-bingo-board :columns columns :rows rows)))

(defun parse-bingo-subsystem (raw-data)
  (let ((numbers (aoc:string-of-numbers->list-of-numbers (caar raw-data) #\,))
        (boards (mapcar #'parse-board (rest raw-data))))
    (make-bingo-subsytem :numbers numbers :boards boards)))

(defun read-data (file)
  (aoc:read-data file
                 :post-process #'(lambda (data)
                                   (parse-bingo-subsystem
                                    (aoc:split-lines-on-empty-line data)))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +part1+
  (read-data (aoc:today-data-pathname)))

(defun find-first-winning-board (numbers boards)
  (labels ((make-state (boards winner number)
             (list boards winner number))
           (state-winner (state) (second state))
           (update-boards (state n)
             (mapcar #'(lambda (b) (mark-number b n)) (first state)))
           (new-winner (boards) (find-if #'winning-board-p boards))
           (non-winners (boards) (remove-if #'winning-board-p boards)))
    (reduce #'(lambda (state n)
                (if (state-winner state) state ;; stop processing if we found a winner
                    (let ((updated (update-boards state n)))
                      (make-state (non-winners updated) (new-winner updated) n))))
            numbers
            :initial-value (make-state boards nil nil))))

(defun find-winning-board (data)
  (let ((numbers (bingo-subsytem-numbers data))
        (boards (bingo-subsytem-boards data)))
    (cdr (find-first-winning-board numbers boards))))

(defun winning-board-score (board last-number)
  (* last-number (apply #'+ (unmarked-numbers board))))

(defun part1 (input)
  (apply #'winning-board-score (find-winning-board input)))

(5am:def-test part1 (:suite :aoc-2021-04)
  (5am:is (= 4512 (part1 +example+)))
  (5am:is (= 8580 (part1 +part1+))))

(defun find-last-winning-board (data)
  (let ((numbers (bingo-subsytem-numbers data))
        (boards (bingo-subsytem-boards data)))
    (labels ((helper (numbers boards winner-and-number)
             (cond ((null boards) winner-and-number)
                   ((null numbers) (error "no winner found!"))
                   (t (let* ((n (first numbers))
                             (updated (mapcar #'(lambda (b) (mark-number b n)) boards))
                             (winner (find-if #'winning-board-p updated))
                             (non-winners (remove-if #'winning-board-p updated)))
                        (helper (rest numbers)
                                non-winners
                                (if winner (list winner n) winner-and-number)))))))
      (helper numbers boards nil))))

(defun part2 (input)
  (apply #'winning-board-score (find-last-winning-board input)))

(5am:def-test part2 (:suite :aoc-2021-04)
  (5am:is (= 1924 (part2 +example+)))
  (5am:is (= 9576 (part2 +part1+))))
