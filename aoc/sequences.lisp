(in-package :aoc)

(defun flatten (list-of-lists)
  (apply #'concatenate 'list list-of-lists))
