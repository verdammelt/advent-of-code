(defpackage #:aoc-2021-06
  (:use :cl))

(in-package #:aoc-2021-06)

(aoc:def-today-suite*)

(defun parse-lanternfish-ages (str)
  (aoc:string-of-numbers->list-of-numbers str :delimiters #\,))

(defun read-data (file) (aoc:read-data file
                                       :line-parser #'parse-lanternfish-ages
                                       :post-process #'first))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +part1+
  (read-data (aoc:today-data-pathname)))

(defun increment-population (population)
  (let ((new-hash (make-hash-table)))
    (maphash #'(lambda (k v)
                 (if (zerop k)
                     (progn (incf (gethash 8 new-hash 0) v)
                            (incf (gethash 6 new-hash 0) v))
                     (incf (gethash (1- k) new-hash 0) v)))
             population)
    new-hash))

(defun do-n-increments (population n)
  (loop for pop = population then (increment-population pop)
        for counter from 1 to n
        finally (return pop)))

(defun init-population (fishes)
  "Creates a hash table that maps from AGE to POPULATION."
  (reduce #'(lambda (h f) (incf (gethash f h 0)) h)
          fishes
          :initial-value (make-hash-table)))

(defun census (population)
  "Returns a total sum of the number of fish"
  (reduce #'+
          (alexandria:hash-table-values population)))

(defun part1 (fishes)
  (census (do-n-increments (init-population fishes) 80)))

(5am:def-test part1 (:suite :aoc-2021-06)
  (5am:is (= 5934 (part1 +example+)))
  (5am:is (= 396210 (part1 +part1+))))

(defun part2 (fishes)
  (census (do-n-increments (init-population fishes) 256)))

(5am:def-test part2 (:suite :aoc-2021-06)
  (5am:is (= 26984457539 (part2 +example+)))
  (5am:is (= 1770823541496 (part2 +part1+))))
