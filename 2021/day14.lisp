(defpackage #:aoc-2021-14
  (:use :cl))

(in-package #:aoc-2021-14)

(aoc:def-today-suite*)

(defun parse-template-rule (rule)
  (aoc:split-string-on-chars '(#\Space #\- #\>) rule))

(defun parse-polymerization-data (lines)
  (let ((template (first lines))
        (rules (subseq lines 2)))
    (list template (mapcar #'parse-template-rule rules))))

(defun read-data (file)
  (aoc:read-data file
                 :post-process #'parse-polymerization-data))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))


(defun maybe-apply-rule (str rules)
  (let ((matching-rule (assoc (subseq str 0 2) rules :test #'string=)))
    (if matching-rule
        (list (subseq str 0 1) (cadr matching-rule))
        (subseq str 0 1))))

(defun polymer-step (template rules)
  (loop for i below (1- (length template))
        appending (maybe-apply-rule (subseq template i) rules) into result
        finally (return (format nil "~{~A~}"
                                (append result (list (subseq template i)))))))

(defun do-polymer-steps (input n)
  (let ((template (first input))
        (rules (second input)))
    (loop for i below n
          for result = (polymer-step template rules)
            then (polymer-step result rules)
          finally (return result))))

(5am:def-test doing-steps (:suite :aoc-2021-14)
  (5am:is (string= "NCNBCHB"
                   (do-polymer-steps +example+ 1)))
  (5am:is (string= "NBCCNBBBCBHCB"
                   (do-polymer-steps +example+ 2)))
  (5am:is (string= "NBBBCNCCNBBNBNBBCHBHHBCHB"
                   (do-polymer-steps +example+ 3)))
  (5am:is (string= "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
                   (do-polymer-steps +example+ 4))))

(defun counts-of-chars (str)
  (reduce
   #'(lambda (counts c) (if (assoc c counts :test #'char=)
                       (progn (incf (cdr (assoc c counts :test #'char=)))
                              counts)
                       (acons c 1 counts)))
   str
   :initial-value (list)))

(defun part1 (input &optional (n 10))
  (let ((char-counts (sort (counts-of-chars (do-polymer-steps input n))
                           #'<
                           :key #'cdr)))
    (- (cdr (first (last char-counts))) ;; most
       (cdr (first char-counts))))) ;; least

(5am:def-test part1 (:suite :aoc-2021-14)
  (5am:is (= 1588 (part1 +example+)))
  (5am:is (= 3048 (part1 +input+))))

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2021-14)
  (5am:skip ":aoc-2021-14.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
