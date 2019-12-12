(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../file-utils")
  (load "../string-utils"))

(defpackage :nbody
  (:use :common-lisp)
  (:export :load-data))

(in-package :nbody)

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
           (split (str) (mapcar #'trim-space (string-utils:split str #\,)))
           (keyword (str) (intern (string-upcase str) :keyword))
           (pair (str) (destructuring-bind (key value) (string-utils:split str #\=)
                         (list (keyword key) (parse-integer value)))))
    (mapcar #'pair (split (unwrap data)))))

(defun parse-moon (data)
  (make-moon (make-coord (parse-coord data))
             (make-zero-coord)))

(defun load-data (file)
  (mapcar #'parse-moon (file-utils:read-lines file)))

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
(assert (= 7722 (total-energy (n-time-steps 1000 (load-data "./input.txt")))))
