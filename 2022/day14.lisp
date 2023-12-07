(defpackage #:aoc-2022-14
  (:use :cl))

(in-package #:aoc-2022-14)

(aoc:def-today-suite*)

;;; TODO: perhaps time for a COORD/POINT abstraction?
(defun parse-coord (str)
  "Parse STR (assumed form: \"X,Y\") into a list of x, y."
  (aoc:string-of-numbers->list-of-numbers str :delimiters #\,))

(defun parse-line-segments (coords)
  "Pairs up the coordinates in COORDS to produce line segments which are a list of
two coordinates."
  (loop for (l1 l2) on coords
        until (null l2)
        collect (list l1 l2)))

(defun parse-line (str)
  (parse-line-segments
   (mapcar #'parse-coord
           (aoc:split-string-on-chars '(#\Space #\- #\>) str))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-line))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun get-feature (cave point)
  (gethash point cave :air))

(defun (setf get-feature) (new-value cave point)
  (setf (gethash point cave) new-value))

(defun place-occupied-p (cave point)
  (not (eq (get-feature cave point) :air)))

(defun create-wall (map line)
  (flet ((draw-vertical-line (p1 p2)
           (loop with x = (first p1)
                 for y from (min (second p1) (second p2)) to (max (second p1) (second p2))
                 do (setf (get-feature map (list x y)) :wall)))
         (draw-horizontal-line (p1 p2)
           (loop with y = (second p1)
                 for x from (min (first p1) (first p2)) to (max (first p1) (first p2))
                 do (setf (get-feature map (list x y)) :wall))))
    (destructuring-bind (p1 p2) line
      (if (= (first p1) (first p2))
          (draw-vertical-line p1 p2)
          (draw-horizontal-line p1 p2)))))

(defun make-cave-map (lines)
  (let ((cave (make-hash-table :test #'equal)))
    (setf (get-feature cave (list 500 0)) :source)
    (loop for l in (aoc:flatten lines)
          do (create-wall cave l))
    cave))

(defun get-ranges-from-cave-map (cave)
  "Collect the x min/max and y min/max from the points in the cave"
  (do ((points (alexandria:hash-table-keys cave) (cdr points))
       (x-max most-negative-fixnum)
       (y-max most-negative-fixnum)
       (x-min most-positive-fixnum)
       (y-min most-positive-fixnum))
      ((null points) (list (list x-min x-max) (list y-min y-max)))
    (destructuring-bind (x y) (first points)
      (setf x-max (max x-max x)
            x-min (min x-min x)
            y-max (max y-max y)
            y-min (min y-min y)))))

(defun pad-ranges (ranges &optional (padding 2))
  "Pad the RANGES (form: '((x-min x-max) (y-min y-max))) by PADDING on all sides"
  (labels ((add-pad-to-range (r) (list (- (first r) padding)
                                       (+ (second r) padding))))
    (mapcar #'add-pad-to-range ranges)))

(defun glyph-for-feature (feature)
  (ecase feature
    (:air #\.)
    (:wall #\#)
    (:sand #\o)
    (:source #\+)))

(defun draw-cave (cave &optional (stream t))
  (destructuring-bind ((x-min x-max) (y-min y-max))
      (pad-ranges (get-ranges-from-cave-map cave))
    (format stream "CAVE: (~Dx~D) [0,0]=[~D,~D]~&"
            (- x-max x-min) (- y-max y-min)
            x-min y-min)
    (loop for y from y-min to y-max
          do (progn (loop for x from x-min to x-max
                          do (format stream "~C"
                                     (glyph-for-feature (get-feature cave (list x y)))))
                    (terpri stream)))))

(defun pour-sand (cave &key floor)
  "Pour a grain of sand in the cave. Report where it comes to rest.

If FLOOR is true then there is a infinite horizontal wall at Y-MAX+2."
  (let ((source (list 500 0)))
    (destructuring-bind ((x-min x-max) (y-min y-max)) (get-ranges-from-cave-map cave)
      (declare (ignore x-min x-max y-min))
      (let ((abyss-y (+ y-max (or floor  0)))
            (floor-y (or floor most-positive-fixnum)))
        (labels ((move-down (p) (list (first p) (1+ (second p))))
                 (move-down-left (p) (list (1- (first p)) (1+ (second p))))
                 (move-down-right (p) (list (1+ (first p)) (1+ (second p))))
                 (occupied-p (p) (or (>= (second p) floor-y) (place-occupied-p cave p)))
                 (into-the-abyss-p (p) (> (second p) abyss-y))
                 (at-rest-p (p) (every #'occupied-p
                                       (list (move-down p)
                                             (move-down-left p)
                                             (move-down-right p)))))
          (do ((point source next-point)
               (next-point source))
              ((or (into-the-abyss-p next-point) (at-rest-p next-point))
               (cond ((equal next-point source) :blocked)
                     ((into-the-abyss-p next-point) :abyss)
                     (t point)))
            (setf next-point (find-if (complement #'occupied-p)
                                      (list (move-down point)
                                            (move-down-left point)
                                            (move-down-right point))))))))))

(defun part1 (input)
  (let ((cave (make-cave-map input)))
    ;; debugging
    ;; (draw-cave cave)
    (do ((grain-count 0 (incf grain-count))
         (poured-grain-pos (pour-sand cave) (pour-sand cave)))
        ((eq poured-grain-pos :abyss) grain-count)
      (setf (get-feature cave poured-grain-pos) :sand)

      ;; debugging
      ;; (draw-cave cave)
      ;; (break)
      )))

(5am:def-test part1 (:suite :aoc-2022-14)
  (5am:is (= 24 (part1 +example+)))
  (5am:is (= 901 (part1 +input+))))

(defun part2 (input)
  "NOTE: (1+ grain-count) because unlike part1 where we know we enter the abyss
after we have; but we know we are blocked right away."
  (let* ((cave (make-cave-map input))
         (ranges (get-ranges-from-cave-map cave))
         (floor-y (+ (second (second ranges)) 2)))
    (do ((grain-count 0 (incf grain-count))
         (poured-grain-pos (pour-sand cave :floor floor-y)
                           (pour-sand cave :floor floor-y)))
        ((eq poured-grain-pos :blocked) (1+ grain-count))
      (setf (get-feature cave poured-grain-pos) :sand)
      ;; (draw-cave cave)
      ;; (break)
      )))

(5am:def-test part2 (:suite :aoc-2022-14)
  (5am:is (= 93 (part2 +example+)))
  (5am:is (= 24589 (part2 +input+))))
