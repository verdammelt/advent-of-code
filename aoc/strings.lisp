(in-package #:aoc)

(defun empty-string-p (str) (zerop (length str)))

(defun split-string-on-char (c str)
  (split-sequence:split-sequence c str :remove-empty-subseqs t))

(defun split-string-on-chars (cs str)
  (split-sequence:split-sequence-if #'(lambda (c) (member c cs))
                                    str :remove-empty-subseqs t))

(defun split-lines-on-empty-line (lines)
  (split-sequence:split-sequence-if #'empty-string-p lines
                                    :remove-empty-subseqs t))

(defun number-string->list-of-digits (str)
  "/e.g/ \"1234\" -> '(1 2 3 4)"
  (map 'list #'digit-char-p str))

;; TODO: add ability to ignore junk when parsing and to take a collection of delimeters
(defun string-of-numbers->list-of-numbers (str &optional (delimiter #\Space))
  "STR is a string containing numbers delimited by DELIMITER"
  (mapcar #'parse-integer (aoc:split-string-on-char delimiter str)))
