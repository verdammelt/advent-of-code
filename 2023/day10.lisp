(defpackage #:aoc-2023-10
  (:use :cl))

(in-package #:aoc-2023-10)

(aoc:def-today-suite*)

(defun list-of-strings->2d-array (lines)
  (aoc:lists->2d-array (mapcar #'(lambda (s) (coerce s 'list)) lines)))

(defun 2d-array->list-of-strings (arr)
  (loop for row below (array-dimension arr 0)
        collect (loop for col below (array-dimension arr 1)
                      collect (aref arr row col) into line
                      finally (return (coerce line 'string)))))

(defun print-map (map &optional (stream t))
  (format stream "~{~A~&~}" (2d-array->list-of-strings map)))

(defun read-data (file) (aoc:read-data file :post-process #'list-of-strings->2d-array))

(defparameter +example+
  (list-of-strings->2d-array
   '("....."
     ".S-7."
     ".|.|."
     ".L-J."
     ".....")))

(defparameter +example-2+
  (list-of-strings->2d-array
   '("-L|F7"
     "7S-7|"
     "L|7||"
     "-L-J|"
     "L|-JF")))

(defparameter +example-3+
  (list-of-strings->2d-array
   '("7-F7-"
     ".FJ|7"
     "SJLL7"
     "|F--J"
     "LJ.LJ")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun make-point (row col) (complex row col))
(defun point-row (point) (realpart point))
(defun point-col (point) (imagpart point))
(defun north (point) (- point #C(1 0)))
(defun south (point) (+ point #C(1 0)))
(defun east (point) (+ point #C(0 1)))
(defun west (point) (- point #C(0 1)))

(defun map-at-point (map point)
  (or (ignore-errors
       (aref map (point-row point) (point-col point)))
      #\.))

(defun connections (map point)
  (let ((at-point (map-at-point map point)))
    (case at-point
      (#\. nil)
      (#\| (list (north point) (south point)))
      (#\- (list (west point) (east point)))
      (#\L (list (north point) (east point)))
      (#\J (list (north point) (west point)))
      (#\7 (list (south point) (west point)))
      (#\F (list (south point) (east point)))
      (#\S (remove nil
                   (list (when (member (map-at-point map (north point)) '(#\| #\7 #\F)) (north point))
                         (when (member (map-at-point map (south point)) '(#\| #\L #\J)) (south point))
                         (when (member (map-at-point map (west point)) '(#\- #\L #\F)) (west point))
                         (when (member (map-at-point map (east point)) '(#\- #\J #\7)) (east point))))))))

(defun find-start (map)
  (aoc:map-2d-array
   #'(lambda (map row col)
       (when (char= (map-at-point map (make-point row col)) #\S)
         (return-from find-start (make-point row col))))
   map))

(defun find-depth (map start)
  (let ((seen (list start)))
    (labels ((next-items (ps)
               (set-difference
                (aoc:flatten (mapcar #'(lambda (p) (connections map p)) ps))
                seen))
             (recurse (next count)
               (setf seen (append next seen))
               (cond ((apply #'= next) (values count seen))
                     (t (recurse (next-items next) (1+ count))))))
      (recurse (connections map start) 1))))

(defun part1 (input)
  (find-depth input (find-start input)))

(5am:def-test part1 (:suite :aoc-2023-10)
  (5am:is (= 4 (part1 +example+)))
  (5am:is (= 4 (part1 +example-2+)))
  (5am:is (= 8 (part1 +example-3+)))
  (5am:is (= 6599 (part1 +input+))))

(defparameter +example-4+
  (list-of-strings->2d-array
   '("..........."
     ".S-------7."
     ".|F-----7|."
     ".||.....||."
     ".||.....||."
     ".|L-7.F-J|."
     ".|..|.|..|."
     ".L--J.L--J."
     "...........")))

(defparameter +example-5+
  (list-of-strings->2d-array
   '(".........."
     ".S------7."
     ".|F----7|."
     ".||....||."
     ".||....||."
     ".|L-7F-J|."
     ".|..||..|."
     ".L--JL--J."
     "..........")))

(defparameter +example-6+
  (list-of-strings->2d-array
   '(".F----7F7F7F7F-7...."
     ".|F--7||||||||FJ...."
     ".||.FJ||||||||L7...."
     "FJL7L7LJLJ||LJ.L-7.."
     "L--J.L7...LJS7F-7L7."
     "....F-J..F7FJ|L7L7L7"
     "....L7.F7||L7|.L7L7|"
     ".....|FJLJ|FJ|F7|.LJ"
     "....FJL-7.||.||||..."
     "....L---J.LJ.LJLJ...")))

(defparameter +example-7+
  (list-of-strings->2d-array
   '("FF7FSF7F7F7F7F7F---7"
     "L|LJ||||||||||||F--J"
     "FL-7LJLJ||||||LJL-77"
     "F--JF--7||LJLJ7F7FJ-"
     "L---JF-JLJ.||-FJLJJ7"
     "|F|F-JF---7F7-L7L|7|"
     "|FFJF7L7F-JF7|JL---7"
     "7-L-JL7||F7|L7F-7F7|"
     "L.L7LFJ|||||FJL7||LJ"
     "L7JLJL-JLJLJL--JLJ.L")))

(defun mark-map (map points mark)
  (mapcar
   #'(lambda (p) (setf (aref map (point-row p) (point-col p)) mark))
   points))

;; attempt at a scanlines implementation - but couldn't get the count correct.
;; (defun mark-non-loop (map)
;;   "MAP here needs to be a map with *only* the loop drawn on it"
;;   (let ((new-map (alexandria:copy-array map)))
;;     (loop for row below (array-dimension map 0)
;;           do (loop for col below (array-dimension map 1)
;;                    for c = (aref map row col) then (aref map row col)
;;                    with in-loop = nil
;;                    when (and in-loop (char= c #\.))
;;                      do (setf (aref new-map row col) #\I)
;;                    when (and (not in-loop) (char= c #\.))
;;                      do (setf (aref new-map row col) #\O)
;;                    when (member c '(#\| #\F #\7 ))
;;                      do (setf in-loop (not in-loop))
;;                    ))
;;     new-map))

(defun deduce-s (map point)
  "POINT is the location of the 'S' on MAP. This returns the actual character of
what is 'under' the 'S'."
  (let ((connections (connections map point)))
    (cond ((equal connections (list (north point) (south point))) #\|)
          ((equal connections (list (west point) (east point))) #\-)
          ((equal connections (list (north point) (west point))) #\J)
          ((equal connections (list (north point) (east point))) #\L)
          ((equal connections (list (south point) (west point))) #\7)
          ((equal connections (list (south point) (east point))) #\F)
          (t (error "whats this?")))))

(defun erase-non-loop (map loop-points)
  (aoc:map-2d-array
   #'(lambda (arr row col)
       (if (member (make-point row col) loop-points)
           (aref arr row col)
           #\.))
   map))

;; IDEA from reddit: magnify map 3x3 and then use flood-fill
(defun enhance! (map)
  (let ((new-map (make-array (mapcar #'(lambda (x) (* x 3)) (array-dimensions map)) :initial-element #\.)))
    (loop for row below (array-dimension map 0)
          do (loop for col below (array-dimension map 1)
                   do (let ((c (aref map row col))
                            (new-row (1+ (* 3 row)))
                            (new-col (1+ (* 3 col))))
                        (when (char= c #\S) (setf c (deduce-s map (make-point row col))))
                        (case c
                          (#\| (loop for d from -1 to 1
                                     do (setf (aref new-map (+ new-row d) new-col) #\|)))
                          (#\- (loop for d from -1 to 1
                                     do (setf (aref new-map new-row (+ new-col d)) #\=)))
                          (#\F (setf (aref new-map new-row (1+ new-col)) #\-
                                     (aref new-map new-row new-col) #\F
                                     (aref new-map (1+ new-row) new-col) #\|))
                          (#\J (setf (aref new-map (1- new-row) new-col) #\|
                                     (aref new-map new-row new-col) #\J
                                     (aref new-map new-row (1- new-col)) #\-))
                          (#\L (setf (aref new-map (1- new-row) new-col) #\|
                                     (aref new-map new-row new-col) #\L
                                     (aref new-map new-row (1+ new-col)) #\-))
                          (#\7 (setf (aref new-map new-row (1- new-col)) #\-
                                     (aref new-map new-row new-col) #\7
                                     (aref new-map (1+ new-row) new-col) #\|))))))
    new-map))

(defun zoom-out (map)
  "Does the opposite of ENHANCE!"
  (let ((new-map (make-array (mapcar #'(lambda (x) (floor x 3)) (array-dimensions map)) :initial-element #\.)))
    (loop for row from 1 below (array-dimension map 0) by 3
          do (loop for col from 1 below (array-dimension map 1) by 3
                   do (let ((new-row (floor row 3))
                            (new-col (floor col 3)))
                        (setf (aref new-map new-row new-col) (aref map row col)))))
    new-map))

(defun count-in-map (map item)
  (loop for idx below (array-total-size map)
        count (char= item (row-major-aref map idx))))

(defun out-of-bounds (map point)
  (or (minusp (point-row point))
      (minusp (point-col point))
      (>= (point-row point) (array-dimension map 0))
      (>= (point-col point) (array-dimension map 1))))

(defun neighbors (point)
  (list (north point) (south point) (west point) (east point)))

;; this is recursing too much on the real input
;; (defun flood-fill (map point fill)
;;   (labels ((recurse (point)
;;              (unless (or (out-of-bounds map point)
;;                          (char/= (map-at-point map point) #\.))
;;                (mark-map map (list point) fill)
;;                (mapc #'recurse (neighbors point)))))
;;     (recurse point)
;;     map))

(defun flood-fill-iter (map point fill)
  (let ((work (list point)))
    (loop for p = (pop work) then (pop work)
          while p
          do (unless (or (out-of-bounds map p)
                         (char/= (map-at-point map p) #\.))
               (mark-map map (list p) fill)
               (setf work (nconc work (neighbors p)))))))


(defun part2 (input)
  (let* ((loop-points (nth-value 1 (find-depth input (find-start input))))
         (loop-only-map (erase-non-loop input loop-points))
         (enhanced (enhance! loop-only-map)))
    (flood-fill-iter enhanced (make-point 0 0) #\O)
    (count-in-map (zoom-out enhanced) #\.)))

(5am:def-test part2 (:suite :aoc-2023-10)
  (5am:is (= 1 (part2 +example+)))
  (5am:is (= 4 (part2 +example-4+)))
  (5am:is (= 4 (part2 +example-5+)))
  (5am:is (= 8 (part2 +example-6+)))
  (5am:is (= 10 (part2 +example-7+)))
  (5am:is (= 477 (part2 +input+))))
