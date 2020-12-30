(defpackage #:aoc-2020-13
  (:use :cl #:aoc))

(in-package #:aoc-2020-13)

(aoc:def-today-suite*)

(defun parse-bus-info (lines)
  (destructuring-bind (target bus-ids) lines
    (list :target (parse-integer target)
          :bus-ids
          (let ((ids (mapcar #'(lambda (s) (parse-integer s :junk-allowed t))
                             (split-sequence:split-sequence #\, bus-ids))))
            (loop  for id in ids
                   for idx below (length ids)
                   when id collect (list id idx))))))

(defun target (info) (getf info :target))
(defun bus-ids (info) (mapcar #'first (getf info :bus-ids)))
(defun bus-ids-with-offset (info) (getf info :bus-ids))

(defparameter +example+ (read-data (today-data-pathname "example") :post-process #'parse-bus-info))
(defparameter +input+ (read-data (today-data-pathname) :post-process #'parse-bus-info))

(defun exact-match-p (id target)
  (zerop (mod target id)))

(defun find-exact-bus (info)
  (let ((target (target info))
        (bus-ids (bus-ids info)))
    (find t
          (mapcar #'(lambda (id) (list id (exact-match-p id target))) bus-ids)
          :key #'second)))

(defun buses-with-next-time-after-target (info)
  (let ((target (target info))
        (bus-ids (bus-ids info)))
    (mapcar #'(lambda (id) (list id (* id (ceiling (/ target id))))) bus-ids)))

(defun next-bus-after-target (ids-and-times)
  (first (sort (copy-seq ids-and-times) #'< :key #'second)))

(defun part1 (input)
  (let ((exact (first (find-exact-bus input))))
    (if exact
        (format t "Surprised! exact match with bus id ~d~&" exact)
        (destructuring-bind (id time)
            (next-bus-after-target (buses-with-next-time-after-target input))
          (* id (- time (target input)))))))

(5am:def-test part1 (:suite :aoc-2020-13)
  (5am:is (= 295 (part1 +example+)))
  (5am:is (= 3882 (part1 +input+))))

(defun first-multiple-at-or-above (n target)
  (* n (ceiling (/ target n))))

(defun all-line-up (time ids)
  (every #'(lambda (id-offset)
             (destructuring-bind (id offset) id-offset
               (zerop (mod (+ time offset) id))))
         ids))

;; (let ((start x) (by (* 1 23 41 647 13 19 29 557 37)) (ids '((23 0) (41 13) (647 23) (13 41) (19 42) (29 52) (557 54) (37 60) (17 71))))
;;                (loop for time from start by by
;;                      when (zerop (mod time 1000000)) do (format t "time: ~D~&" time)
;;                      when (all-line-up time ids) do (return time)))

(defun find-next-time (start step buses)
  (loop for time from start by step
        when (all-line-up time buses)
          do (return time)))

(defun compute-step (some-ids)
  (reduce #'* (butlast some-ids) :key #'first :initial-value 1))

(defun part2 (input)
  (let* ((ids (bus-ids-with-offset input)))
    (do* ((idx 1 (+ 1 idx))
          (some-ids (subseq ids 0 idx) (subseq ids 0 idx))
          (step (compute-step some-ids) (compute-step some-ids))
          (start (first (first ids)) (find-next-time start step some-ids)))
         ((>= idx (length ids)) start))))

(5am:def-test part2 (:suite :aoc-2020-13)
  (5am:is (= 1068781 (part2 +example+)))
  (5am:is (= 3417 (part2 (parse-bus-info '("0" "17,x,13,19")))))
  (5am:is (= 754018 (part2 (parse-bus-info '("0" "67,7,59,61")))))
  (5am:is (= 779210 (part2 (parse-bus-info '("0" "67,x,7,59,61")))))
  (5am:is (= 1261476 (part2 (parse-bus-info '("0" "67,7,x,59,61")))))
  (5am:is (= 1202161486 (part2 (parse-bus-info '("0" "1789,37,47,1889")))))

  (5am:is (= 867295486378319 (part2 +input+))))
