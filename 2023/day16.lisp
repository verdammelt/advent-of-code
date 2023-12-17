(defpackage #:aoc-2023-16
  (:use :cl))

(in-package #:aoc-2023-16)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file :post-process #'aoc:lists->2d-array))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

;; beam = coord + direction
(defun make-beam (coord direction)
  (cons coord direction))
(defun beam-coord (beam) (car beam))
(defun beam-dir (beam) (cdr beam))
(defun beam-funcall (beam fn)
  "FUNCALLs FN to BEAM providing beam coord and beam direction to FN"
  (destructuring-bind (coord . dir) beam
    (funcall fn coord dir)))

(defun make-coord (x y) (cons x y))
(defun coord-less-p (c1 c2)
  (if (= (car c1) (car c2))
      (< (cdr c1) (cdr c2))
      (< (car c1) (car c2))))
(defun coord-equal (c1 c2) (equalp c1 c2))
(defun out-of-bounds-p (bounds coord)
  (destructuring-bind (x . y) coord
    (destructuring-bind (max-x max-y) bounds
      (or (minusp x) (minusp y)
          (>= x max-x) (>= y max-y)))))

(defun left (coord) (cons (car coord) (1- (cdr coord))))
(defun right (coord) (cons (car coord) (1+ (cdr coord))))
(defun up (coord) (cons (1- (car coord)) (cdr coord)))
(defun down (coord) (cons (1+ (car coord)) (cdr coord)))

(defun dir->fn (dir)
  (ecase dir
    (:left #'left)
    (:right #'right)
    (:up #'up)
    (:down #'down)))

(defun move-beam (beam)
  (beam-funcall
   beam
   #'(lambda (coord dir) (make-beam (funcall (dir->fn dir) coord) dir))))

(defun mirror-backslash (coord dir)
  (list
   (ecase dir
     (:left (make-beam coord :up))
     (:right (make-beam coord :down))
     (:up (make-beam coord :left))
     (:down (make-beam coord :right)))))

(defun mirror-slash (coord dir)
  (list
   (ecase dir
     (:left (make-beam coord :down))
     (:right (make-beam coord :up))
     (:up (make-beam coord :right))
     (:down (make-beam coord :left)))))

(defun splitter-vertical (coord dir)
  (ecase dir
    ((:left :right) (list (make-beam coord :up)
                          (make-beam coord :down)))
    ((:up :down) (list (make-beam coord dir)))))

(defun splitter-horizontal (coord dir)
  (ecase dir
    ((:left :right) (list (make-beam coord dir)))
    ((:up :down) (list (make-beam coord :left)
                       (make-beam coord :right)))))

(defun modify-beam (beam tile)
  (ecase tile
    (#\. (list beam))
    (#\\ (beam-funcall beam #'mirror-backslash))
    (#\/ (beam-funcall beam #'mirror-slash))
    (#\| (beam-funcall beam #'splitter-vertical))
    (#\- (beam-funcall beam #'splitter-horizontal))))

(defun energize-tile (energized coord)
  "Adds the COORD to the list of energize tiles if not already present. Keeps the
list sorted"
  (setf (gethash coord energized) t)
  energized)

(defun num-energized (energized)
  (hash-table-count energized))

(defun tile-at (layout coord)
  (aref layout (car coord) (cdr coord)))

(defparameter *seen-beams* (make-hash-table :test #'equalp))
(defun clear-seen-beams () (clrhash *seen-beams*))
(defun saw-beam (beam)
  (incf (gethash beam *seen-beams* 0)))
(defun deja-vu-beam-p (beam)
  (gethash beam *seen-beams*))

(defun ok-beam (beam layout)
  (and beam
       (not (out-of-bounds-p (array-dimensions layout) (beam-coord beam)))))

(defun tick (layout beams energized)
  "Move the BEAMS one space in their current direction on LAYOUT. Apply any changes
due to splitters/mirrors. Keep track of cells touched by a beam in ENERGIZED.
Evalutate to values: (BEAMS ENERGIZED)"
  (let ((result-beams (list)))
    ;; (format t "~& BEAMS: ~S" beams)
    (dolist (beam beams)
      (let ((beam (move-beam beam)))
        (when (ok-beam beam layout)
          (dolist (b (modify-beam beam (tile-at layout (beam-coord beam))))
            (when (ok-beam b layout)
              (push b result-beams))))))
    (setf result-beams (remove-if #'deja-vu-beam-p (remove-duplicates result-beams :test #'coord-equal))
          energized (reduce #'energize-tile
                            (mapcar #'beam-coord result-beams)
                            :initial-value energized))
    (mapc #'saw-beam result-beams)
    (values result-beams energized)))

(defun tick-until-settled (layout first-beam)
  "Initially call TICK with initial BEAM and empty ENERGIZED list. Then repeatedly
call it until the energized list appears to have 'settled' (_i.e._ has not
changed in N ticks)"
  (do ((energized (make-hash-table :test #'equalp))
       (beams (list first-beam))
       (tick-count 0) ;; for debugging
       (settling-count 0)
       (settling-threshold 10))
      ((or (null beams) (>= settling-count settling-threshold))
       (values beams energized))

    (let ((old-count (hash-table-count energized)))
      (multiple-value-bind (new-beams new-energized)
          (tick layout beams energized)

        (incf tick-count)

        (if (= old-count (hash-table-count new-energized))
            (incf settling-count)
            (setf settling-count 0))

        (setf beams new-beams
              energized new-energized)))))

(defun draw-beams-on-layout (layout energized)
  (let ((copy (alexandria:copy-array layout)))
    (loop for c in energized
          do (setf (aref copy (car c) (cdr c)) #\#))
    (aoc:print-2d-array copy)))

(defun num-energized-with-initial-beam (layout init-beam)
  (clear-seen-beams)
  (multiple-value-bind (beams energized)
      (tick-until-settled layout init-beam)
    (declare (ignore beams))
    (num-energized energized)))

(defun part1 (input)
  (num-energized-with-initial-beam input (make-beam (make-coord 0 -1) :right)))

(5am:def-test part1 (:suite :aoc-2023-16)
  (5am:is (= 46 (part1 +example+)))
  (5am:is (= 7199 (part1 +input+))))

(defun part2 (input)
  (destructuring-bind (max-row max-col) (array-dimensions input)
    (loop for row below max-row
          for col below max-col
          maximize (num-energized-with-initial-beam
                    input (make-beam (make-coord row -1) :right))
          maximize (num-energized-with-initial-beam
                    input (make-beam (make-coord row max-col) :left))
          maximize (num-energized-with-initial-beam
                    input (make-beam (make-coord -1 col) :down))
          maximize (num-energized-with-initial-beam
                    input (make-beam (make-coord max-row col) :up)))))

(5am:def-test part2 (:suite :aoc-2023-16)
  (5am:is (= 51 (part2 +example+)))
  (5am:is (= 7438 (part2 +input+))))
