(defpackage #:aoc-2020-04
  (:use :cl #:aoc-2020/utils))

(in-package #:aoc-2020-04)

(aoc:def-today-suite*)

(defun parse-passport (passport)
  (mapcar #'(lambda (s) (aoc:split-string-on-char #\: s))
          (aoc:split-string-on-char #\Space passport)))

(defun read-passport-data (&optional modifier)
  (flet ((pre-process (lines)
           (mapcar #'(lambda (strs) (join-strings " " strs))
                   (aoc:split-lines-on-empty-line lines))))
    (aoc:read-data (aoc:today-data-pathname modifier)
                   :pre-process #'pre-process
                   :line-parser #'parse-passport)))

(defparameter +input+ (read-passport-data))
(defparameter +example+ (read-passport-data "example" ))
(defparameter +invalid+ (read-passport-data "invalid"))
(defparameter +valid+ (read-passport-data "valid"))

(defun always-valid (str) (declare (ignore str)) t)

(defun four-digit-between (min max)
  (lambda (str)
    (and (= (length str) 4)
         (<= min (parse-number str) max))))

(defun hex-color (str)
  (and (= (length str) 7)
       (char= (char str 0) #\#)
       (ignore-errors (parse-integer (subseq str 1) :radix 16 :junk-allowed nil))))

(defun height (str)
  (multiple-value-bind (num extra)
                       (parse-number str)
                       (cond ((string= extra "cm") (<= 150 num 193))
                             ((string= extra "in") (<= 59 num 76))
                             (t nil))))

(defun eye-color (str)
  (member str '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))

(defun passport-id (str)
  (and (= (length str) 9)
       (ignore-errors (parse-integer str :junk-allowed nil))))

(defun parse-number (str)
  (multiple-value-bind (num junk-idx) (parse-integer str :junk-allowed t)
                       (values num (subseq str junk-idx))))

(defparameter *fields*
  `(("byr" . ,(four-digit-between 1920 2002))
    ("iyr" . ,(four-digit-between 2010 2020))
    ("eyr" . ,(four-digit-between 2020 2030))
    ("hgt" . ,#'height)
    ("hcl" . ,#'hex-color)
    ("ecl" . ,#'eye-color)
    ("pid" . ,#'passport-id)
    ("cid" . ,#'always-valid)))

(defun validate-field (field value)
  (funcall (cdr (assoc field *fields* :test #'string=)) value))

(defun required-fields (fields)
  "All fields but CID are required"
  (remove "cid" (mapcar #'car fields) :test #'string=))

(defun get-passport-field (passport field)
  (cadr (assoc field passport :test #'string=)))

(defun has-all-fields (required-fields)
  (lambda (passport)
    (let* ((keys (mapcar #'first passport))
           (missing (set-difference required-fields keys :test #'string=)))
      (values (zerop (length missing)) missing))))

(defun has-valid-fields (fields)
  (lambda (passport)
    (reduce #'(lambda (valid field)
                (when valid
                  (validate-field field (get-passport-field passport field))))
            (mapcar #'first fields)
            :initial-value t)))

(defun compose-validators (&rest validators)
  (lambda (passport)
    (reduce #'(lambda (valid fn) (when valid (funcall fn passport)))
            validators
            :initial-value t)))

(defun valid-passports (passports validator)
  (remove-if-not validator passports))

(5am:test part1
  (5am:is (= 2 (length (valid-passports
                        +example+
                        (has-all-fields (required-fields *fields*))))))
  (5am:is (= 250 (length (valid-passports
                          +input+
                          (has-all-fields (required-fields *fields*)))))))

(5am:test part2
  (5am:is (= 158 (length (valid-passports
                          +input+
                          (compose-validators
                           (has-all-fields (required-fields *fields*))
                           (has-valid-fields *fields*)))))))
