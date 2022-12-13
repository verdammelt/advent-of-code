(defpackage #:aoc-2022-13
  (:use :cl))

(in-package #:aoc-2022-13)

(aoc:def-today-suite*)

(defun parse-bracket-list (str)
  (read-from-string
   (substitute #\Space #\,
               (substitute #\) #\]
                           (substitute #\( #\[ str)))))

(defun parse-bracket-lists (strs)
  (mapcar #'parse-bracket-list strs))

(defun read-data (file) (aoc:read-data file
                                       :pre-process #'aoc:split-lines-on-empty-line
                                       :line-parser #'parse-bracket-lists))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun compare (left right)
  (cond (;; if integer or lists equal skip to the next part
         (equal left right) :skip)

        ;; integer compare
        ((and (integerp left) (integerp right)) (< left right))

        ;; coersion cases
        ((integerp left) (compare (list left) right))
        ((integerp right) (compare left (list right)))

        ;; null LEFT < null RIGHT
        ((null left) t)
        ((null right) nil)

        ;; if CARs compare as SKIP go to CDR otherwise use whatever CARs campare to
        (t (let ((car-compare (compare (car left) (car right))))
             (if (eq car-compare :skip) (compare (cdr left) (cdr right)) car-compare)))))

(5am:test compare-signals
  ;; encoding the instructions
  (5am:is (compare 1 2))
  (5am:is (not (compare 2 1)))
  (5am:is (eq (compare 1 1) :skip)) ;; testing implementation :(
  (5am:is (compare '(1) '(2)))
  (5am:is (not (compare '(2) '(1))))
  (5am:is (compare '(1 1) '(2 2)))
  (5am:is (not (compare '(1 2) '(1 1))))
  (5am:is (eq (compare '(1 1) '(1 1)) :skip))
  (5am:is (compare '(1 2 10) '(1 3 0))) ;; stop at first item that works
  (5am:is (compare '(1) '(1 1)))
  (5am:is (not (compare '(1 1) '(1))))
  (5am:is (compare 1 '(2)))
  (5am:is (compare '(1) 2))

  ;; examples from the instructions
  (5am:is (compare '(1 1 3 1 1) '(1 1 5 1 1)))
  (5am:is (compare '((1) (2 3 4)) '((1) 4)))
  (5am:is (not (compare '(9) '((8 7 6)))))
  (5am:is (compare '((4 4) 4 4) '((4 4) 4 4 4)))
  (5am:is (not (compare '(7 7 7 7) '(7 7 7))))
  (5am:is (compare '() '(3)))
  (5am:is (not (compare '((())) '(()))))
  (5am:is (not (compare '(1 (2 (3 (4 (5 6 7)))) 8 9) '(1 (2 (3 (4 (5 6 0)))) 8 9))))

  ;; extra cases
  (5am:is (not (compare '((1) 3) '((1) 2)))) ;; sublists equal so keep checking
  )

(defun apply-fn (fn) (lambda (arglist) (apply fn arglist)))

(defun part1 (input)
  (aoc:sum
   (mapcar #'cdr
           (remove nil
                   (mapcar #'cons
                           (mapcar (apply-fn #'compare) input)
                           (loop for i from 1 below (1+ (length input)) collect i))
                   :key #'car))))

;; works for example but not input :(
;; need to find corner cases in COMPARE that are not being handled properly

(5am:def-test part1 (:suite :aoc-2022-13)
  (5am:is (= 13 (part1 +example+)))
  (5am:is (= 5760 (part1 +input+))))

(defun part2 (input)
  (let* ((special-packets (list '((2)) '((6))))
         (all-packets (apply #'append special-packets (copy-tree input)))
         (sorted (sort all-packets #'compare)))

    (aoc:product
     (mapcar #'(lambda (p) (1+ (position p sorted))) special-packets))))

(5am:def-test part2 (:suite :aoc-2022-13)
  (5am:is (= 140 (part2 +example+)))
  (5am:is (= 26670 (part2 +input+))))
