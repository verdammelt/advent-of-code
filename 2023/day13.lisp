(defpackage #:aoc-2023-13
  (:use :cl))

(in-package #:aoc-2023-13)

(aoc:def-today-suite*)

(defun print-field (field &optional  (stream t))
  (loop for row below (array-dimension field 0)
        do (fresh-line stream)
        do (loop for col below (array-dimension field 1)
                 do (princ (aref field row col) stream))))

(defun parse-fields (fields)
  (mapcar #'aoc:lists->2d-array (aoc:split-lines-on-empty-line fields)))

(defun read-data (file) (aoc:read-data file :post-process #'parse-fields))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun find-possible-mirror (field)
  (loop for dir in '(:row :col)
        for max-dim in (array-dimensions field)
        append
        (loop for idx below (1- max-dim)
              when (equalp (aoc:slice-2d-array field dir idx)
                           (aoc:slice-2d-array field dir (1+ idx)))
                collect (list dir idx (1+ idx)))))

(defun check-possible-mirror (field mirror)
  (destructuring-bind (dir left right) mirror
    (let ((min 0)
          (max (if (eq dir :row) (array-dimension field 0) (array-dimension field 1))))
      (loop for left from left downto min
            for right from right below max
            always (equalp (aoc:slice-2d-array field dir left)
                           (aoc:slice-2d-array field dir right))))))

(defun check-possible-mirrors (field mirrors)
  (remove-if-not #'(lambda (m) (check-possible-mirror field m)) mirrors))

(defun find-mirrors (field)
  (check-possible-mirrors field (find-possible-mirror field)))

(defun find-mirror (field)
  (first (find-mirrors field)))

(defun mirror-value (mirror)
  (destructuring-bind (dir left right) mirror
    (declare (ignore right))
    (* (1+ left) (if (eq dir :row) 100 1))))

(defun part1 (input)
  (reduce #'+ (mapcar #'find-mirror input)
          :initial-value 0
          :key #'mirror-value))

(5am:def-test part1 (:suite :aoc-2023-13)
  (5am:is (= 405 (part1 +example+)))
  (5am:is (= 43614 (part1 +input+))))

(defun fix-smudge (field idx)
  (let ((fix '((#\. . #\#)
               (#\# . #\.))))
    (setf (row-major-aref field idx)
          (cdr (assoc (row-major-aref field idx) fix)))))

(defun all-possible-smudge-fixes (field)
  (loop for idx below (array-total-size field)
        collect (let ((copy (alexandria:copy-array field )))
                  (fix-smudge copy idx)
                  copy)))

(defun find-mirror-after-smudge-fix (field)
  (let* ((no-smudge-mirror (find-mirror field))
         (all-possible-fixes (all-possible-smudge-fixes field))
         (all-possible-post-fix-mirrors
           (remove-duplicates
            (aoc:flatten (mapcar #'find-mirrors all-possible-fixes))
            :test #'equal)))
    (first (remove no-smudge-mirror all-possible-post-fix-mirrors :test #'equal))))

(defun part2 (input)
  (reduce #'+ (mapcar #'find-mirror-after-smudge-fix input)
          :initial-value 0
          :key #'mirror-value))

(5am:def-test part2 (:suite :aoc-2023-13)
  (5am:is (= 400 (part2 +example+)))
  (5am:is (= 36771 (part2 +input+))))
