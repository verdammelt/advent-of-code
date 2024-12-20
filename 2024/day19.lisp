(defpackage #:aoc-2024-19
  (:use :cl))

(in-package #:aoc-2024-19)

(aoc:def-today-suite*)

(defun read-data (file)
  (let ((data
          (aoc:read-data
           file
           :line-parser #'(lambda (s) (aoc:split-string-on-chars '(#\, #\Space) s))
           :post-process #'aoc:split-lines-on-empty-line)))
    (list (aoc:flatten (first data))
          (aoc:flatten (second data)))
    ))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defparameter *find-arrangement-memo* (make-hash-table :test #'equalp))

;; 2 problems with this for part 2
;; 1) results are deeply nested - so simple counting not possible
;; 2) memoization is causing the same 'solution' to be returned anytime there is a solution
;; TODO: [2024-12-19] Fix this function so we can use it for part2
(defun find-arrangement (pattern towels arrangement)
  (or (gethash pattern *find-arrangement-memo*)
      (setf (gethash pattern *find-arrangement-memo*)
            (cond ((alexandria:emptyp pattern) arrangement)
                  (t (remove nil
                             (mapcar
                              #'(lambda (towel)
                                  (multiple-value-bind (matchp suffix)
                                      (alexandria:starts-with-subseq towel pattern :return-suffix t)
                                    (when matchp
                                      (find-arrangement suffix towels (cons towel arrangement)))
                                    ))
                              towels)))))))

(defun part1 (input)
  (clrhash *find-arrangement-memo*)
  (destructuring-bind (towels patterns) input
    (count-if #'(lambda (pattern) (find-arrangement pattern towels (list))) patterns)))

(5am:def-test part1 (:suite :aoc-2024-19)
  (5am:is (= 6 (part1 +example+)))
  (5am:is (= 371 (part1 +input+))))

(defun part2 (input)
  (destructuring-bind (towels patterns) input
    (mapcar #'(lambda (pattern) (find-arrangement pattern towels (list))) patterns)))

(5am:def-test part2 (:suite :aoc-2024-19)
  (5am:skip ":aoc-2024-19.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
