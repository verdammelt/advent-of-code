(defpackage #:aoc-2022-06
  (:use :cl))

(in-package #:aoc-2022-06)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :post-process #'first))

(defparameter +examples+ '("mjqjpqmgbljsphdztnvjfqwrcgsmlb"
                           "bvwbjplbgvbhsrlpgdmjqwftvncz"
                           "nppdvjthqldpwncqszvftbrmjlhg"
                           "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
                           "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun all-different-p (chars)
  (= (length chars) (length (remove-duplicates chars))))

(defun find-marker (signal count)
  "A marker is found when COUNT characters of SIGNAL are all different"
  (loop for n below (- (length signal) count)
        when (all-different-p (subseq signal n (+ n count)))
          do (return (list (+ count n) (subseq signal n (+ n count)))))  )

(defun find-start-of-packet-marker (signal)
  (find-marker signal 4))

(defun part1 (input)
  (first (find-start-of-packet-marker input)))

(5am:def-test part1 (:suite :aoc-2022-06)
  (5am:is (equal '(7 5 6 10 11) (mapcar #'part1 +examples+)))
  (5am:is (= 1275 (part1 +input+))))

(defun find-start-of-message-marker (signal)
  (find-marker signal 14))

(defun part2 (input)
  (first (find-start-of-message-marker input)))

(5am:def-test part2 (:suite :aoc-2022-06)
  (5am:is (equal '(19 23 23 29 26) (mapcar #'part2 +examples+)))
  (5am:is (= 3605 (part2 +input+))))
