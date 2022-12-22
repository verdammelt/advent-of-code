(in-package :aoc)

(defun keywordize (str)
  "Convert STR into a keyword with STR as symbol-name. Unconditionally upcases the
given string."
  (intern (string-upcase str) :keyword))

(defun number-or-keyword (str)
  "Convert STR into a number (if possible) or a keyword."
  (handler-case (parse-integer str)
    (error () (keywordize str))))
