(defpackage #:aoc-2022-22
  (:use :cl))

(in-package #:aoc-2022-22)

(aoc:def-today-suite*)

(defun string-pad-right (str len &optional (char #\Space))
  (let ((pad-len (- len (length str))))
    (cond ((minusp pad-len) (error "PAD-LEN is negative!"))
          ((zerop pad-len) str)
          (t (concatenate 'string str
                          (make-string pad-len :initial-element char))))))

(defun read-data (file) (aoc:read-data file :post-process #'aoc:split-lines-on-empty-line))

(defun parse-map (the-map)
  (let ((max-width (reduce #'max (mapcar #'length the-map))))
    (aoc:lists->2d-array (mapcar #'(lambda (s) (string-pad-right s max-width #\Space)) the-map))))

(defun parse-movements (movements)
  (let ((str (first movements)))
    (do ((idx 0)
         (num nil)
         (collected nil))
        ((>= idx (length str)) (reverse collected))
      (multiple-value-bind (maybe-num new-idx)
          (parse-integer str :start idx :junk-allowed t)
        (if maybe-num
            (progn (push maybe-num collected)
                   (setf idx new-idx))
            (progn (push (aoc:keywordize (char str idx)) collected)
                   (setf idx (1+ idx))))))))

(defun parse-input (input)
  (destructuring-bind (the-map movements) input
      (list :map (parse-map the-map)
            :moves (parse-movements movements)
            :position (list 0 (position #\. (first the-map)))
            :direction :>)))

;; TODO: will need update for cubic representation (maybe)
(defun out-of-bounds-p (map coord)
  (destructuring-bind (max-row max-col) (array-dimensions map)
    (destructuring-bind (row col) coord
      (or (minusp row) (minusp col)
          (>= row max-row) (>= col max-col)))))

(defun wall-p (map coord)
  (and (not (out-of-bounds-p map coord))
       (char= (apply #'aref map coord) #\#)))

;; TODO: will need update for cubic representation
(defun off-edge-p (map coord)
  (or (out-of-bounds-p map coord)
      (char= (apply #'aref map coord) #\Space)))

(defun direction->delta (direction)
  (ecase direction
    (:> '(0 1))
    (:< '(0 -1))
    (:^ '(-1 0))
    (:v '(1 0))))

(defun apply-delta (coord delta)
  (list (+ (first coord) (first delta))
        (+ (second coord) (second delta))))

;; TODO: will need update for cubic representation
(defun first-empty-space-opposite-side (map pos direction)
  (do ((row (case direction
              (:^ (1- (array-dimension map 0)))
              (:V 0)
              (t (first pos))))
       (col (case direction
              (:< (1- (array-dimension map 1)))
              (:> 0)
              (t (second pos))))
       (row-delta (first (direction->delta direction)))
       (col-delta (second (direction->delta direction)))
       (found nil))
      ((or (>= row (array-dimension map 0))
           (>= col (array-dimension map 1))
           found)
       (if found
           (values found direction)
           (error "fell off row/col looking for empty space!")))

    (let ((cell (aref map row col)))
      (cond ((char= cell #\Space)
             (incf row row-delta)
             (incf col col-delta))
            ((char= cell #\#)
             ;; (warn "oh no a wall! act like nothing happened")
             (setf found pos))
            ((char= cell #\.)
             (setf found (list row col)))
            (t (error "WTF cell is this? ~D ~D" row col))))))

(defvar *move-trace* nil
  "Keep track of each position and direction as we move around the map. This will
be in reverse order with latest move first.")

(defun move (map start steps direction)
  (push (list start direction) *move-trace*)
  (do ((count 0 (1+ count))
       (dir direction)
       (pos start))
      ((>= count steps) (values pos direction))
    (let ((next (apply-delta pos (direction->delta dir))))
      (cond ((wall-p map next) (setf count steps))
            ((off-edge-p map next)
             (multiple-value-setq (pos dir)
               (first-empty-space-opposite-side map pos dir)))
            (t (setf pos next)))

      (push (list pos direction) *move-trace*))))

(defun turn (current left-right)
  (let ((right-turns '(:^ :> :V :< :^))
        (left-turns '(:^ :< :V :> :^)))
    (flet ((one-turn (turns) (nth (1+ (position current turns)) turns)))
      (ecase left-right
        (:L (one-turn left-turns))
        (:R (one-turn right-turns))))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun do-move (map move pos dir)
  (if (numberp move)
      (move map pos move dir)
      (values pos (turn dir move))))

(defvar *the-map* nil
  "The parsed map of the latest run.")

(defun make-moves (parsed-input)
  (let ((the-map (getf parsed-input :map)))
    (setq *the-map* the-map) ;; for debuggin/tracing
    (do ((moves (getf parsed-input :moves) (rest moves))
         (pos (getf parsed-input :position))
         (dir (getf parsed-input :direction)))
        ((null moves) (values pos dir))
      (multiple-value-setq (pos dir)
        (do-move the-map (first moves) pos dir)))))

(defun draw-map (map)
  (dotimes (row (array-dimension map 0))
    (dotimes (col (array-dimension map 1))
      (format t "~C" (aref map row col)))
    (format t "~&")))

(defun copy-map (map)
  (let ((new-map (make-array (array-dimensions map))))
    (dotimes (row-major-idx (array-total-size map))
      (setf (row-major-aref new-map row-major-idx)
            (row-major-aref map row-major-idx)))
    new-map))

(defun apply-trace-to-map (map trace)
  "Apply data from TRACE to MAP to create a new map with the moves shown."
  (let ((new-map (copy-map map)))
    (dolist (step (reverse trace))
      (destructuring-bind (pos dir) step
        (setf (aref new-map (first pos) (second pos))
              (ecase dir
                (:^ #\^)
                (:> #\>)
                (:V #\V)
                (:< #\<)))))
    new-map))

(defun part1 (input)
  (setq *move-trace* nil *the-map* nil)
  (multiple-value-bind (pos direction) (make-moves (parse-input input))
    (+ (* 1000 (1+ (first pos)))
       (* 4 (1+ (second pos)))
       (ecase direction
         (:> 0)
         (:v 1)
         (:< 2)
         (:^ 3)))))

(5am:def-test part1 (:suite :aoc-2022-22)
  (5am:is (= 6032 (part1 +example+)))
  (5am:is (= 191010 (part1 +input+))))

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2022-22)
  (5am:skip ":aoc-2022-22.2 not implemented")
  ;; (5am:is (= -1 (part2 +input+)))
  )
