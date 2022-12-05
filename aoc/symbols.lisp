(in-package :aoc)

(defun keywordize (str)
  "Convert STR into a keyword with STR as symbol-name. Unconditionally upcases the
given string."
  (intern (string-upcase str) :keyword))
