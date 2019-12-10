(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../file-utils"))

(defpackage :monitor-station
  (:use :common-lisp)
  (:export))

(in-package :monitor-station)

(defparameter *empty-space* #\.)

(defun parse-data-row (y row)
  (loop
     :for x :from 0 :below (length row)
       :collect `(,(char row x) (,x ,y))))

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
   (list :best '(3 4)
         :count 8
         :data (load-map "./test-1.txt"))
   (list :best '(5 8)
         :count 33
         :data (load-map "./test-2.txt"))
   (list :best '(1 2)
         :count 35
         :data (load-map "./test-3.txt"))
   (list :best '(6 3)
         :count 41
         :data (load-map "./test-4.txt"))
   (list :best '(11 13)
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

(defun slope-intercept-fn (point1 point2)
  (let ((m (/ (- (second point1) (second point2))
              (- (first point1) (first point2))))
        (b (second point1)))
    #'(lambda (point) (+ (* m (first point)) b))))
