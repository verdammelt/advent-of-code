(defpackage #:aoc-2024-14
  (:use :cl))

(in-package #:aoc-2024-14)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file
                 :line-parser
                 #'(lambda (str) (aoc:string-of-numbers->list-of-numbers
                             str :delimiters '(#\Space #\= #\,) :junk-allowed t))
                 :post-process #'(lambda (bots) (mapcar #'(lambda (bot) (remove nil bot)) bots))))

(defun parse-robot-data (data)
  (reduce
   #'(lambda (grid bot)
       (let ((pos (aoc:make-coord (first bot) (second bot)))
             (vel (aoc:make-coord (third bot) (fourth bot))))
         (push vel (gethash pos grid (list)))
         grid))
   data
   :initial-value (make-hash-table :test #'equalp)))

(defun move-robots (grid)
  "Move all robots on GRID to their next location. Returns a new grid"
  (let ((dimensions (getf grid :dimensions))
        (robots (getf grid :robots))
        (new-robots (make-hash-table :test #'equalp)))
    (labels ((maybe-teleport (coord)
             "if COORD is outside of DIMENSIONS then teleport to correct new location"
             (aoc:make-coord (mod (aoc:coord-x coord) (first dimensions))
                             (mod (aoc:coord-y coord) (second dimensions))))
           (move-robot (pos vel)
             "move robot from pos to a new pos. add it at the new position to new-robots"
             (let ((new-pos (maybe-teleport (aoc:coord-add pos vel))))
               (push vel (gethash new-pos new-robots (list))))))

      (maphash
       #'(lambda (pos bots) (mapcar #'(lambda (vel) (move-robot pos vel)) bots))
       robots)

      (list :dimensions dimensions :robots new-robots))))

(defun print-grid (grid &key (stream t))
  "Because my 2d grids are x=up/down and y=left/right this reverses the
dimensions and coords"
  (let* ((dimensions (reverse (getf grid :dimensions)))
         (robots (getf grid :robots))
         (grid-array (make-array dimensions :initial-element #\.)))

    (maphash
     #'(lambda (pos bots) (setf (aoc:coord-aref grid-array (reverse pos))
                           (digit-char (length bots))))
     robots)

    (aoc:print-2d-array grid-array :stream stream)))

(defparameter +example+
  (list :dimensions (list 11 7)
        :robots (parse-robot-data
                 (read-data (aoc:today-data-pathname "example")))))

(defparameter +input+
  (list :dimensions (list 101 103)
        :robots (parse-robot-data (read-data (aoc:today-data-pathname)))))

(defun time-travel (grid num-seconds)
  (do ((s 0 (incf s))
       (grid grid (move-robots grid)))
      ((>= s num-seconds) grid)))

(defun get-quandrants (grid)
  (let ((dimensions (getf grid :dimensions))
        (robots-in-quandrants (make-list 4 :initial-element 0)))
    (flet ((pos->quad (pos)
             (let ((quad-x-size (floor (first dimensions) 2))
                   (quad-y-size (floor (second dimensions) 2))
                   (pos-x (aoc:coord-x pos))
                   (pos-y (aoc:coord-y pos)))

               (cond ((and (< pos-x quad-x-size) (< pos-y quad-y-size)) 0)
                     ((and (> pos-x quad-x-size) (> pos-y quad-y-size)) 3)
                     ((and (> pos-x quad-x-size) (< pos-y quad-y-size)) 1)
                     ((and (< pos-x quad-x-size) (> pos-y quad-y-size)) 2)
                     (t nil)))))
      (maphash #'(lambda (pos bots)
                   (let ((quad-num (pos->quad pos)))
                     (when quad-num
                       (incf (nth quad-num robots-in-quandrants) (length bots)))))
               (getf grid :robots))
      robots-in-quandrants)))

(defun compute-safety-factor (grid)
  (aoc:product (get-quandrants grid)))

(defun part1 (input)
  (let ((100-seconds-later (time-travel input 100)))
    (aoc:product (get-quandrants 100-seconds-later))))

(5am:def-test part1 (:suite :aoc-2024-14)
  (5am:is (= 12 (part1 +example+)))
  (5am:is (= 210587128 (part1 +input+))))

(defun part2 (input)
  ;; use 'safety factor' to find out when most of the grid is blank
  ;; this is when it is likely that the robots have formed up into a Christmas tree.
  ;; note that pattern will loop every (lcm 101 103) seconds
  (loop with min-safety = most-positive-fixnum
        with min-n = -1
        for n below (lcm 101 103)
        for grid = input then (move-robots grid)
        for safety = (compute-safety-factor grid)
          then (compute-safety-factor grid)
        when (< safety min-safety)
          do (setf min-safety safety min-n n)
        finally (return min-n)))


(5am:def-test part2 (:suite :aoc-2024-14)
  (5am:is (= 7286 (part2 +input+))))
