(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../file-utils"))

(defpackage :monitor-station
  (:use :common-lisp)
  (:export))

(in-package :monitor-station)

;;; ---------- ASTEROID ----------

(defclass asteroid () ((x :initarg :x :reader x) (y :initarg :y :reader y)))
(defun make-asteroid (x y) (make-instance 'asteroid :x x :y y))

(defmethod print-object ((object asteroid) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "[~A, ~A]" (x object) (y object))))

;;; ---------- PARSING ----------

(defparameter *empty-space* #\.)

(defun parse-data-row (y row)
  (loop
     :for x :from 0 :below (length row)
       :collect `(,(char row x) ,(make-asteroid x y))))

(defun find-asteroids (data)
  (mapcar #'second
          (remove *empty-space*
                  (loop
                     :for y :from 0 :below (length data)
                     :append (parse-data-row y (nth y data)))
                  :key #'first)))

(defun load-map (file)
  (find-asteroids (file-utils:read-lines file)))

;;; ---------- TEST & PUZZLE DATA ----------

;;
;; test-1 - 3,4 - 8
;; test-2 - 5,8 - 33
;; test-3 - 1,2 - 35
;; test-4 - 6,3 - 41
;; test-5 - 11,13 - 210
(defparameter *maps*
  (mapcar #'load-map
          '("./test-1.txt" "./test-2.txt" "./test-3.txt" "./test-4.txt" "./test-5.txt"
            "./input.txt")))

(defun slope-between-asteroids (a1 a2)
  (atan (- (x a2) (x a1))
        (- (y a2) (y a1))))

(defun find-slopes-from (a1 map)
  (let ((slopes
         (reduce
          #'(lambda (slopes a2) (unless (eq a1 a2) (incf (gethash (slope-between-asteroids a1 a2) slopes 0))) slopes)
          map
          :initial-value (make-hash-table))))
    slopes))

(defun best-asteroid-for-base (map)
  (first
   (sort (mapcar #'(lambda (a) (list a (hash-table-count (find-slopes-from a map)))) map)
         #'>
         :key #'second)))

;; part 1 answer is 286 - however the above code was giving 285
;; noticed an off-by-one problem in 3rd-6th test data.
