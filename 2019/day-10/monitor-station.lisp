(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../file-utils"))

(defpackage :monitor-station
  (:use :common-lisp)
  (:export))

(in-package :monitor-station)

(defparameter *empty-space* #\.)

(defclass asteroid () ((x :initarg :x :reader x) (y :initarg :y :reader y)))
(defun make-asteroid (x y) (make-instance 'asteroid :x x :y y))

(defmethod print-object ((object asteroid) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "[~A, ~A]" (x object) (y object))))


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

(defun make-map (asteroid-list)
  (reduce #'(lambda (map coord)
              (setf (gethash coord map) t)
              map)
          asteroid-list
          :initial-value (make-hash-table :test 'equal)))

(defun load-map (file)
  (let* ((raw-data (file-utils:read-lines file))
         (dimensions (list (length (first raw-data))
                           (length raw-data))))
    (list :dimensions dimensions
          :map (make-map (find-asteroids raw-data)))))

(defparameter *test-data*
  (list
   (list :best (make-asteroid 3 4)
         :count 8
         :data (load-map "./test-1.txt"))
   (list :best (make-asteroid 5 8)
         :count 33
         :data (load-map "./test-2.txt"))
   (list :best (make-asteroid 1 2)
         :count 35
         :data (load-map "./test-3.txt"))
   (list :best (make-asteroid 6 3)
         :count 41
         :data (load-map "./test-4.txt"))
   (list :best (make-asteroid 11 13)
         :count 210
         :data (load-map "./test-5.txt"))))

(defparameter *puzzle-input* (load-map "./input.txt"))

(defun map-asteroids (map fn)
  (loop for k being the hash-key of map collect (funcall fn k)))

(defun all-asteroids (map)
  (map-asteroids map #'identity))

;; for every asteroid
;; - for all remaining asteroids
;;   - make a line to this second asteroid
;;   - collect all other asteroids on this line (any asteroid on the line 'hides' the others on the line
;;   - remove these asteroids from consideration
;; - count up these collections of asteroids
;; - find asteroid with the large number of these collections.
;;
;; problem: points asteroid in the middles has other asteroids on either side of it
;; on the same line but need to be counted differently... how?
;; (thought: maybe treat each asteroid in first loop as (0 0) then positive/negative
;;  can be handled separately?)

(defun slope-between-asteroids (a1 a2)
  (atan (- (x a2) (x a1))
        (- (y a2) (y a1))))

(defun find-slopes-from (a1 map)
  (let ((slopes
         (reduce
          #'(lambda (slopes a2) (incf (gethash (slope-between-asteroids a1 a2) slopes 0)) slopes)
          (all-asteroids map)
          :initial-value (make-hash-table))))
    (remhash 0.0 slopes)
    slopes))

(defun best-asteroid-for-base (map)
  (first
   (sort (map-asteroids map #'(lambda (a) (list a (hash-table-count (find-slopes-from a map)))))
         #'>
         :key #'second)))

;; part 1 answer is 286 - however the above code was giving 285
;; noticed an off-by-one problem in 3rd-6th test data.
