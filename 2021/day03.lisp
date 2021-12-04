(defpackage #:aoc-2021-03
  (:use :cl))

(in-package #:aoc-2021-03)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +part1+
  (read-data (aoc:today-data-pathname)))

(defun parse-binary-integer (str) (parse-integer str :radix 2))

(defun group-by (list &key (key #'identity) (test #'eql))
  (reduce #'(lambda (h item)
              (push item (gethash (funcall key item) h (list)))
              h)
          list
          :initial-value (make-hash-table :test test)))

(defun group-by-char-at-index (list index)
  (group-by list :key #'(lambda (s) (char s index))))

(defun greater-of-the-two (hash key1 key2)
  "Return the key that has more items in the hash as its value, returns KEY1 if they are equal."
  (let ((val1-len (length (gethash key1 hash)))
        (val2-len (length (gethash key2 hash))))
    (if (>= val1-len val2-len) key1 key2)))

(defun lesser-of-the-two (hash key1 key2)
  "Return the key that has less items in the hash as its value, returns KEY1 if they are equal."
  (let ((val1-len (length (gethash key1 hash)))
        (val2-len (length (gethash key2 hash))))
    (if (<= val1-len val2-len) key1 key2)))

(defun choose-most-common (choices index)
  (greater-of-the-two (group-by-char-at-index choices index) #\1 #\0))

(defun choose-least-common (choices index)
  (lesser-of-the-two (group-by-char-at-index choices index) #\0 #\1))

(defun compute-gamma (readings)
  (parse-binary-integer
   (loop for index below (length (first readings))
         collect (choose-most-common readings index) into chars
         finally (return (concatenate 'string chars)))))

(defun compute-epsilon (readings)
  (parse-binary-integer
   (loop for index below (length (first readings))
         collect (choose-least-common readings index) into chars
         finally (return (concatenate 'string chars)))))

(defun compute-power-consumption (input)
  (* (compute-gamma input)
     (compute-epsilon input)))

(defun part1 (input)
  (compute-power-consumption input))

(5am:def-test part1 (:suite :aoc-2021-03)
  (5am:is (= 22 (compute-gamma +example+)))
  (5am:is (= 9 (compute-epsilon +example+)))
  (5am:is (= 198 (part1 +example+)))
  (5am:is (= 2261546 (part1 +part1+))))

(defun char-at-index-p (char index) (lambda (s) (char= (char s index) char)))

(defun filter-for-key-reading (choices index indicator-chooser)
  "Filter CHOICES by looking for a indicator at INDEX of each. Indicator is
determined by calling INDICATOR-CHOOSER. Recursively filters choices until one remains."
  (cond ((null choices) nil)
        ((= 1 (length choices)) (first choices))
        (t
         (let ((indicator (funcall indicator-chooser choices index)))
           (filter-for-key-reading
            (remove-if-not (char-at-index-p indicator index) choices)
            (1+ index)
            indicator-chooser)))))

(defun compute-oxygen-generator-rating (input)
  (parse-binary-integer (filter-for-key-reading input 0 #'choose-most-common)))

(defun compute-co2-scrubber-rating (input)
  (parse-binary-integer (filter-for-key-reading input 0 #'choose-least-common)))

(defun compute-life-support-rating (input)
  (* (compute-oxygen-generator-rating input)
     (compute-co2-scrubber-rating input)))

(defun part2 (input)
  (compute-life-support-rating input))

(5am:def-test part2 (:suite :aoc-2021-03)
  (5am:is (= 23 (compute-oxygen-generator-rating +example+)))
  (5am:is (= 10 (compute-co2-scrubber-rating +example+)))
  (5am:is (= 230 (part2 +example+)))
  (5am:is (= 6775520 (part2 +part1+))))
