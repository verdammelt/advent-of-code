(defpackage #:aoc-2020-15
  (:use :cl #:aoc))

(in-package #:aoc-2020-15)

(5am:def-suite :aoc-2020-15 :in :aoc-2020)

(defun parsed-data (&optional label)
  (flet ((split-on-comma (s) (split-sequence:split-sequence #\, s)))
   (read-data (today-data-pathname label)
              :pre-process (lambda (ss) (mapcan #'split-on-comma ss))
              :line-parser #'parse-integer)))

(defparameter +example+ (parsed-data "example"))

(defparameter +input+ (parsed-data))

(defstruct memory-game
  (numbers (make-hash-table) :read-only t)
  (last-number nil))

(defun add-number (game number turn)
  (let ((value (gethash number (memory-game-numbers game) (list nil nil))))
    (setf (gethash number (memory-game-numbers game)) (list turn (car value))
          (memory-game-last-number game) number))
  game)

(defun init-game (game starting-numbers)
  (do ((turn 1 (1+ turn))
       (numbers starting-numbers (cdr numbers)))
      ((> turn (length starting-numbers)) game)
    (add-number game (car numbers) turn)))

(defun get-number (game number)
  (gethash number (memory-game-numbers game)))

(defun decide-next-number (game)
  (let ((last-number (memory-game-last-number game)))
    (destructuring-bind (last-time time-before &rest ignore)
        (get-number game last-number)
      (declare (ignore ignore))
      (if time-before (- last-time time-before) 0))))

(defun nth-number (input max-turn)
  (let ((game (init-game (make-memory-game) input)))
    (do ((turn (1+ (length input)) (1+ turn)))
        ((> turn max-turn) game)
      (let ((next-number (decide-next-number game)))
        (add-number game next-number turn)))

    (memory-game-last-number game)))

(defun part1 (input)
  (nth-number input 2020))

(5am:def-test part1 (:suite :aoc-2020-15)
  (5am:is (= 436 (part1 +example+)))
  (5am:is (= 387 (part1 +input+))))

(defun part2 (input)
  (nth-number input 30000000))

(5am:def-test part2 (:suite :aoc-2020-15)
  (format 5am:*test-dribble* "(WARNING: VERY SLOW) ")

  (macrolet ((gc-then-test (&body tests)
             `(progn (sb-ext:gc :full t)
                       ,@tests)))

    (gc-then-test
     (5am:is (= 175594 (part2 +example+))))
    (gc-then-test
     (5am:is (= 2578 (part2 '(1 3 2)))))

    (gc-then-test
     (5am:is (= 3544142 (part2 '(2 1 3)))))
    (gc-then-test
     (5am:is (= 261214 (part2 '(1 2 3)))))
    (gc-then-test
     (5am:is (= 6895259 (part2 '(2 3 1)))))
    (gc-then-test
     (5am:is (= 18 (part2 '(3 2 1)))))
    (gc-then-test
     (5am:is (= 362 (part2 '(3 1 2)))))

    (gc-then-test
     (5am:is (= 6428 (part2 +input+))))

)
)
