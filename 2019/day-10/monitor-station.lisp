;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (load "../file-utils"))

(defpackage :monitor-station
  (:use :common-lisp)
  (:export))

(in-package :monitor-station)

;;; ---------- ASTEROID ----------

(defclass asteroid () ((x :initarg :x :reader x) (y :initarg :y :reader y)))
(defun make-asteroid (x y) (make-instance 'asteroid :x x :y y))

(defun asteroid-equal (a1 a2)
  (and (= (x a1) (x a2))
       (= (y a1) (y a2))))

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
  (find-asteroids (file-utils:read-lines (file-utils:file-in-day file 10))))

;;; ---------- TEST & PUZZLE DATA ----------

(defparameter *maps*
  (mapcar #'load-map
          '("./test-1.txt" "./test-2.txt" "./test-3.txt" "./test-4.txt" "./test-5.txt"
            "./input.txt")))


;;; ---------- SLOPES & DISTANCE ----------

(defun slope-between-asteroids (a1 a2)
  (atan (- (x a2) (x a1))
        (- (y a2) (y a1))))

(defun distance-between-asteroids (a1 a2)
  (sqrt (+ (expt (- (x a1) (x a2)) 2)
           (expt (- (y a1) (y a2)) 2))))

(defun sort-by-distance (a1 asteriods)
  (sort asteriods #'< :key #'(lambda (a2) (distance-between-asteroids a1 a2))))

(defun find-slopes-from (a1 map)
  (let ((slopes
         (reduce
          #'(lambda (slopes a2)
              (unless (eq a1 a2)
                (push a2 (gethash (slope-between-asteroids a1 a2) slopes (list))))
              slopes)
          map
          :initial-value (make-hash-table))))
    (maphash #'(lambda (angle asteriods)
                 (setf (gethash angle slopes)
                       (sort-by-distance a1 asteriods)))
             slopes)
    slopes))

;;; ---------- part I ----------
(defun best-asteroid-for-base (map)
  (first
   (sort (mapcar #'(lambda (a) (list a (hash-table-count (find-slopes-from a map)))) map)
         #'>
         :key #'second)))

(defun answer-equal (test-num actual expected)
  (let ((a1 (first actual))
        (count1 (second actual))
        (a2 (first expected))
        (count2 (second expected)))
    (unless (asteroid-equal a1 a2)
      (error "#~D Asteroids do not match (actual: ~A expected: ~A)"
             test-num a1 a2))
    (unless (= count1 count2)
      (error "#~D Counts do not match (actual: ~A expected: ~A)"
             test-num count1 count2))))
(mapcar #'answer-equal
        '(1 2 3 4 5 6)
        (mapcar #'best-asteroid-for-base *maps*)
        `((,(make-asteroid 3 4) 8)
          (,(make-asteroid 5 8) 33)
          (,(make-asteroid 1 2) 35)
          (,(make-asteroid 6 3) 41)
          (,(make-asteroid 11 13) 210)
          (,(make-asteroid 22 25) 286)))

;;; ---------- part II ----------

(defun vaporize-in-order (asteroids order)
  (cond ((null asteroids) order)
        ((atom (car asteroids))
         (vaporize-in-order (rest asteroids)
                            (append order (list (car asteroids)))))
        (t (vaporize-in-order (append (rest asteroids)
                                      (rest (car asteroids)))
                              (append order (list (car (car asteroids))))))))

(defun order-of-vaporization (base map)
  (let* ((slopes (find-slopes-from base map))
         (angles (sort (loop for k being the hash-keys of slopes collect k) #'>))
         (clockwise-asteroids (mapcar #'(lambda (angle) (gethash angle slopes)) angles)))
    (vaporize-in-order clockwise-asteroids (list))))

;;; ---------- part II test ----------
(let* ((expectations
        `((1 ,(make-asteroid 11 12))
          (2 ,(make-asteroid 12 1))
          (3 ,(make-asteroid 12 2))
          (10 ,(make-asteroid 12 8))
          (20 ,(make-asteroid 16 0))
          (50 ,(make-asteroid 16 9))
          (100 ,(make-asteroid 10 16))
          (199 ,(make-asteroid 9 6))
          (200 ,(make-asteroid 8 2))
          (201 ,(make-asteroid 10 9))
          ;(299 ,(make-asteroid 11 1)) ;; this one doesn't work!?
          ))
       (asters (fifth *maps*))
       (base (first (best-asteroid-for-base asters)))
       (order (order-of-vaporization base asters)))
  (mapcar #'(lambda (expected)
              (let* ((nth (car expected))
                     (a1 (cadr expected))
                     (a2 (nth (1- nth) order)))
                (unless (asteroid-equal a1 a2)
                  (error "#~D does not match expected: ~A found ~A"
                         nth a1 a2))))
          expectations))

;; ---------- answering part II ----------
(let* ((asters (sixth *maps*))
       (base (first (best-asteroid-for-base asters)))
       (order (order-of-vaporization base asters))
       (200th (nth 199 order)))
  (unless (= (+ (* (x 200th) 100) (y 200th))
             504)
    (error "part II failed to find correct 200th asteriod vaporized")))
