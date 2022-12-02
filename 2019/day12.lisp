(defpackage #:aoc-2019-12
  (:use :cl))

(in-package #:aoc-2019-12)

(aoc:def-today-suite*)

;;; ---------- coordinates (x, y, z) ----------
(defun make-coord (coords)
  (apply #'append coords))
(defun make-zero-coord ()
  (make-coord '((:x 0) (:y 0) (:z 0))))
(defun coord-x (coord) (getf coord :x))
(defun coord-y (coord) (getf coord :y))
(defun coord-z (coord) (getf coord :z))
(defun coord-values (coord) (list (coord-x coord)
                                  (coord-y coord)
                                  (coord-z coord)))

;;; ---------- moon (location, velocity) ----------
(defun make-moon (location velocity)
  (list :location location :velocity velocity))
(defun moon-loc (moon) (getf moon :location))
(defun moon-vel (moon) (getf moon :velocity))
(defun moon-pot (moon) (apply #'+ (mapcar #'abs (coord-values (moon-loc moon)))))
(defun moon-kin (moon) (apply #'+ (mapcar #'abs (coord-values (moon-vel moon)))))
(defun moon-energy (moon) (* (moon-pot moon)
                             (moon-kin moon)))

;;; ---------- data file ----------

(defun parse-coord (data)
  (labels ((unwrap (str) (string-trim '(#\< #\>) str))
           (trim-space (str) (string-trim '(#\Space) str))
           (split (str) (mapcar #'trim-space (aoc:split-string-on-char #\, str)))
           (keyword (str) (intern (string-upcase str) :keyword))
           (pair (str) (destructuring-bind (key value) (aoc:split-string-on-char #\= str)
                         (list (keyword key) (parse-integer value)))))
    (mapcar #'pair (split (unwrap data)))))

(defun parse-moon (data)
  (make-moon (make-coord (parse-coord data))
             (make-zero-coord)))

(defun load-data (file)
  (mapcar #'parse-moon (aoc:read-data file)))

(defun gravity-between (loc1 loc2)
  (list :x (signum (- (coord-x loc2) (coord-x loc1)))
        :y (signum (- (coord-y loc2) (coord-y loc1)))
        :z (signum (- (coord-z loc2) (coord-z loc1)))))

(defun sum-vectors (&rest vectors)
  (reduce #'(lambda (acc v) (list :x (+ (coord-x acc) (coord-x v))
                             :y (+ (coord-y acc) (coord-y v))
                             :z (+ (coord-z acc) (coord-z v))))
          vectors
          :initial-value (make-zero-coord)))

(defun gravity-delta (positions)
  (let ((gravity-vectors (loop for pos in positions
                            collect (loop for other in positions
                                       collect (gravity-between pos other)))))
    (mapcar #'(lambda (vectors) (apply #'sum-vectors vectors)) gravity-vectors)))

(defun time-step (moons)
  (let* ((positions (mapcar #'moon-loc moons))
         (affect-of-gravity (gravity-delta positions))
         (velocities (mapcar #'moon-vel moons))
         (new-velocities (mapcar #'sum-vectors affect-of-gravity velocities))
         (new-positions (mapcar #'sum-vectors positions new-velocities))
         (new-moons (mapcar #'make-moon new-positions new-velocities)))
    new-moons))

(defun total-energy (moons)
  (apply #'+ (mapcar #'moon-energy moons)))

(defun n-time-steps (n moons)
  (loop
     for i from 0 below n
     for new-moons = (time-step moons) then (time-step new-moons)
     finally (return new-moons)))

;;; ---------- part I ----------
(5am:def-test part1 (:suite :aoc-2019-12)
  (5am:is (= 9743 (total-energy (n-time-steps 1000 (load-data (aoc:today-data-pathname)))))))

(defun find-cycle (initial)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (do ((moons (time-step initial) (time-step moons))
       (count 1 (+ count 1)))
      ((equal moons initial) count)))

(defun find-cycle-axis (initial accessor)
  (let ((start (funcall accessor initial)))
    (do ((moons (time-step initial) (time-step moons))
         (count 1 (+ 1 count)))
        ((equal start (funcall accessor moons)) count))))

;;; this is not giving the correct info...
(defun find-cycle-lcm (initial)
  (flet ((get-axis (axis) (lambda (ms) (mapcar #'(lambda (m) (funcall axis (moon-loc m))) ms))))
    (lcm (find-cycle-axis initial (get-axis #'coord-x))
         (find-cycle-axis initial (get-axis #'coord-y))
         (find-cycle-axis initial (get-axis #'coord-z)))))

(defun find-zero-velocity (initial)
  (let ((zero-velocity (make-zero-coord)))
   (do ((moons (time-step initial) (time-step moons))
        (count 1 (+ count 1)))
       ((every #'(lambda (m) (equal zero-velocity (moon-vel m))) moons) (* 2 count)))))

(defun zero-velocities (moons axis)
  (every #'zerop (mapcar axis (mapcar #'moon-vel moons))))

;; this one works.
(defun from-reddit (initial)
  (do ((moons (time-step initial) (time-step moons))
       (count 1 (+ count 1))
       (x-period nil)
       (y-period nil)
       (z-period nil))
      ((and x-period y-period z-period)
       (* 2 (lcm x-period y-period z-period)))
    (if (and (not x-period) (zero-velocities moons #'coord-x))
        (setf x-period count))
    (if (and (not y-period) (zero-velocities moons #'coord-y))
        (setf y-period count))
    (if (and (not z-period) (zero-velocities moons #'coord-z))
        (setf z-period count))))

(defun run-the-test (datafile cycle-finder)
  (let ((data (load-data datafile)))
    (sb-ext:gc :full t)
    (funcall cycle-finder data)))

(5am:def-test part2 (:suite :aoc-2019-12)
  (5am:is (= 288684633706728 (run-the-test (aoc:today-data-pathname) #'from-reddit))))
