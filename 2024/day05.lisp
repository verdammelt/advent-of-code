(defpackage #:aoc-2024-05
  (:use :cl))

(in-package #:aoc-2024-05)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data
   file
   :line-parser #'(lambda (l) (mapcar #'parse-integer
                                 (aoc:split-string-on-chars '(#\| #\,) l)))
   :post-process #'aoc:split-lines-on-empty-line))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun rules (input) (first input))
(defun updates (input) (second input))

(defun pages-to-be-before (page rules)
  (mapcar #'second (remove-if-not #'(lambda (p) (= page p)) rules :key #'first)))

(defun update-valid-p (update rules)
  (labels ((%update-valid-p (update rules)
             (let* ((first (first update))
                    (rest (rest update))
                    (pages-to-be-before (pages-to-be-before first rules)))
               (cond ((null rest) t)
                     ((intersection rest pages-to-be-before) nil)
                     (t (%update-valid-p rest rules))))))
    (%update-valid-p (reverse update) rules)))

(defun middle (list)
  (nth (floor (/ (length list) 2)) list))

(defun part1 (input)
  (let ((correctly-ordered
          (remove-if-not #'(lambda (u) (update-valid-p u (rules input)))
                         (updates input))))
    (aoc:sum (mapcar #'middle correctly-ordered))))

(5am:def-test update-valid-p (:suite :aoc-2024-05)
  (let ((rules (rules +example+)))
    (5am:is (update-valid-p '(75 47 61 53 29) rules))
    (5am:is (update-valid-p '(97 61 53 29 13) rules))
    (5am:is (update-valid-p '(75 29 13) rules))
    (5am:is (not (update-valid-p '(75 97 47 61 53) rules)))
    (5am:is (not (update-valid-p '(61 13 29) rules)))
    (5am:is (not (update-valid-p '(97 13 75 29 47) rules)))))

(5am:def-test part1 (:suite :aoc-2024-05)
  (5am:is (= 143 (part1 +example+)))
  (5am:is (= 5275 (part1 +input+))))

(defun fix-ordering (update rules)
  (sort (copy-seq update)
        #'(lambda (x y) (member y (pages-to-be-before x rules)))))

(defun part2 (input)
  (let* ((rules (rules input))
         (incorrect-ordered (remove-if #'(lambda (u) (update-valid-p u rules)) (updates input))))
    (aoc:sum
     (mapcar #'middle
      (mapcar #'(lambda (u) (fix-ordering u rules)) incorrect-ordered)))))

(5am:def-test fix-ordering (:suite :aoc-2024-05)
  (let ((rules (rules +example+)))
    (5am:is (equal '(97 75 47 61 53) (fix-ordering '(75 97 47 61 53) rules)))
    (5am:is (equal '(61 29 13) (fix-ordering '(61 13 29) rules)))
    (5am:is (equal '(97 75 47 29 13) (fix-ordering '(97 13 75 29 47) rules)))))

(5am:def-test part2 (:suite :aoc-2024-05)
  (5am:is (= 123 (part2 +example+)))
  (5am:is (= 6191 (part2 +input+))))
