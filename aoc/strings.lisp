(in-package #:aoc)

(defun empty-string-p (str) (zerop (length str)))

(defun split-string-on-char (c str)
  (split-sequence:split-sequence c str :remove-empty-subseqs t))

(defun split-string-on-chars (cs str)
  (split-sequence:split-sequence-if #'(lambda (c) (member c cs))
                                    str :remove-empty-subseqs t))

(defun split-lines-on-empty-line (lines)
  (split-sequence:split-sequence-if #'empty-string-p lines))
