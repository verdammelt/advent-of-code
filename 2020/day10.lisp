(defpackage #:aoc-2020-10
  (:use :cl #:aoc-2020/utils #:aoc))

(in-package #:aoc-2020-10)

(5am:def-suite :aoc-2020-10 :in :aoc-2020)

(defun outlet-joltage (jolts) (declare (ignore jolts)) 0)
(defun device-joltage (jolts) (+ 3 (first (last jolts))))

(defun add-outlet-joltage (jolts) (append (list (outlet-joltage jolts)) jolts))
(defun add-device-joltage (jolts) (append jolts (list (device-joltage jolts))))

(defun post-process-jolts (jolts)
  (add-outlet-joltage
   (add-device-joltage
    (sort (copy-seq jolts) #'<))))

(defparameter +input+ (read-data (today-data)
                                 :line-parser #'parse-integer
                                 :post-process #'post-process-jolts))
(defparameter +example+ (read-data (today-data "example")
                                   :line-parser #'parse-integer
                                   :post-process #'post-process-jolts))
(defparameter +example-longer+ (read-data (today-data "example-longer")
                                          :line-parser #'parse-integer
                                          :post-process #'post-process-jolts))

(defun diff-pairs (nums)
  (when (and (first nums) (second nums)) (- (second nums) (first nums))))

(defun pair-wise-diffs (jolts)
  (butlast (maplist #'diff-pairs jolts)))

(defun diff-distribution (jolts)
  (let* ((diffs (pair-wise-diffs jolts))
         (uniq-diffs (remove-duplicates diffs)))

    (reduce #'(lambda (dist diff) (acons diff (count diff diffs) dist))
            uniq-diffs :initial-value (list))))


(5am:def-test part1 (:suite :aoc-2020-10)
  (let ((dist (diff-distribution +example+)))
    (5am:is (= 7 (cdr (assoc 1 dist))))
    (5am:is (= 5 (cdr (assoc 3 dist)))))

  (let ((dist (diff-distribution +example-longer+)))
    (5am:is (= 22 (cdr (assoc 1 dist))))
    (5am:is (= 10 (cdr (assoc 3 dist)))))

  (let ((dist (diff-distribution +input+)))
    (5am:is (= 68 (cdr (assoc 1 dist))))
    (5am:is (= 32 (cdr (assoc 3 dist))))

    ;; the answer to part 1
    (5am:is (= 2176 (* (cdr (assoc 1 dist))
                       (cdr (assoc 3 dist)))))))


;;
;; part II
;;
;; Not "solved" so much as implenting the community chosen answer.
;; 1) get the list of joltage-diffs
;; 2) split those on difference of 3 (thise *must* be chosen - no choices)
;; 3) The number of possible paths is based upon length of the grouping:
;; 4) the number of possible paths is the tribonnacci(length)
;; 5) the total number of paths is the product of all of the above.

(let ((memo-hash (make-hash-table)))
  (defun tribonacci (n)
    (let ((memoized (gethash n memo-hash)))
      (if memoized memoized
          (setf (gethash n memo-hash)
                (cond
                  ((= n 0) 1)
                  ((= n 1) 1)
                  ((= n 2) 2)
                  (t (+ (tribonacci (- n 1))
                        (tribonacci (- n 2))
                        (tribonacci (- n 3)))))))))
  (defun trib-memo () memo-hash)
  (defun trib-clear () (clrhash memo-hash)))

(defun number-of-arrangements (jolts)
  (let ((sub-diffs (split-sequence:split-sequence 3 (pair-wise-diffs jolts))))
    (reduce #'* (mapcar #'tribonacci (mapcar #'length sub-diffs)))))

(5am:def-test part2 (:suite :aoc-2020-10)
  (5am:is (= 8 (number-of-arrangements +example+)))
  (5am:is (= 19208 (number-of-arrangements +example-longer+)))
  (5am:is (= 18512297918464 (number-of-arrangements +input+))))
