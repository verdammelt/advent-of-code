(defpackage #:aoc-2024-15
  (:use :cl))

(in-package #:aoc-2024-15)

(aoc:def-today-suite*)

(defun parse-map (map)
  (let ((grid (make-hash-table :test #'equalp))
        (array (aoc:lists->2d-array map)))
    (aoc:map-2d-array #'(lambda (m x y) (setf (gethash (aoc:make-coord x y) grid)
                                         (aref m x y)))
                      array)
    grid))

(defun print-grid (grid)
  (if (not (typep grid 'hash-table)) (print-grid (getf grid :map))
      (let* ((size (ceiling (sqrt (hash-table-count grid))))
             (array (make-array (list size size))))
        (maphash #'(lambda (pos char) (setf (aoc:coord-aref array pos) char)) grid)
        (aoc:print-2d-array array))))

(defun parse-map-and-instructions (lines)
  (destructuring-bind (map instructions) (aoc:split-lines-on-empty-line lines)
    (list :map (parse-map map)
          :instructions (coerce (apply #'concatenate 'string instructions) 'list))))

(defun read-data (file)
  (aoc:read-data file :post-process #'parse-map-and-instructions))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +example-small+
  (read-data (aoc:today-data-pathname "example-small")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun make-move (grid position dir)
  (let* ((next-position (aoc:coord-add position dir))
         (next-thing (gethash next-position grid #\#)))
    (flet ((update-grid (the-grid)
             (setf (gethash next-position the-grid) (gethash position the-grid)
                   (gethash position the-grid) #\.)
             (values the-grid t)))
      (case next-thing
        (#\# (values grid nil))
        (#\O (multiple-value-bind (new-grid moved-p)
                 (make-move grid next-position dir)
               (if moved-p (update-grid new-grid) (values grid nil))))
        (#\. (update-grid grid))
        (t (error "unknown next thing: ~S~&" next-thing))))))

(defun instruction->dir (instruction)
  (case instruction
    (#\^ (aoc:make-coord -1 0))
    (#\> (aoc:make-coord 0 1))
    (#\< (aoc:make-coord 0 -1))
    (#\v (aoc:make-coord 1 0))))

(defun find-bot (grid)
  (let ((bot-pos nil))
    (maphash #'(lambda (pos char) (when (char= char #\@) (setf bot-pos pos))) grid)
    bot-pos))

(defun follow-instructions (grid instructions)
  (let ((grid (alexandria:copy-hash-table grid)))
    (dolist (instruction instructions grid)
      (setf grid (make-move grid (find-bot grid)
                            (instruction->dir instruction)))
      )))

(defun find-boxes (grid)
  (let ((boxes (list)))
    (maphash #'(lambda (pos char) (when (char= char #\O) (push pos boxes))) grid)
    boxes))

(defun gps-coordinate (pos)
  (+ (* 100 (aoc:coord-x pos)) (aoc:coord-y pos)))

(defun part1 (input)
  (let* ((grid (getf input :map))
         (instructions (getf input :instructions))
         (final-grid (follow-instructions grid instructions)))
    (aoc:sum (mapcar #'gps-coordinate (find-boxes final-grid)))))

(5am:def-test part1 (:suite :aoc-2024-15)
  (5am:is (= 2028 (part1 +example-small+)))
  (5am:is (= 10092 (part1 +example+)))
  (5am:is (= 1360570 (part1 +input+))))

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2024-15)
  (5am:skip ":aoc-2024-15.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
