(in-package :aoc)

(defun keywordize (str)
  "Convert STR into a keyword with STR as symbol-name. Unconditionally upcases the
given string."
  (alexandria:make-keyword (string-upcase str)))

(defun number-or-keyword (str)
  "Convert STR into a number (if possible) or a keyword."
  (handler-case (parse-integer str)
    (error () (keywordize str))))
