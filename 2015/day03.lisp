(defpackage #:aoc-2015-03
  (:use :cl))

(in-package #:aoc-2015-03)

(aoc:def-today-suite*)

(defun parse-line (l) (coerce l 'list))

(defun read-data (file) (aoc:read-data file
                                       :line-parser #'parse-line
                                       :post-process #'first))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defstruct (point (:constructor point (x y)))
  (x) (y))

(defun point-add (p1 p2)
  (point (+ (point-x p1) (point-x p2))
         (+ (point-y p1) (point-y p2))))

(defun travel (current move)
  (point-add current (ecase move
                       (#\^ (point 1 0))
                       (#\v (point -1 0))
                       (#\> (point 0 1))
                       (#\< (point 0 -1)))))

(defun visit (position houses)
  (incf (gethash position houses 0))
  houses)

(defun visit-houses (movements &optional (houses (make-hash-table :test #'equalp)))
  (let ((current (point 0 0)))
    (reduce #'(lambda (houses move) (visit (setf current (travel current move)) houses))
            movements
            :initial-value (visit current houses))))

(defun count-houses-visited (movements)
  (hash-table-count (visit-houses movements)))

(defun part1 (input) (count-houses-visited input))

(5am:def-test part1 (:suite :aoc-2015-03)
  (5am:is (= 2 (part1 (parse-line ">"))))
  (5am:is (= 4 (part1 (parse-line "^>v<"))))
  (5am:is (= 2 (part1 (parse-line "^v^v^v^v^v"))))

  (5am:is (= 2572 (part1 +input+))))

(defun deal-list-into-two-hands (list)
  (loop
     :for idx :below (length list)
     :if (evenp idx)
     :collect (nth idx list) :into santa
     :else
     :collect (nth idx list) :into robot
     :finally (return (list santa robot))))

(defun share-route-with-robosanta (movements)
  (destructuring-bind (santa-route robot-route)
      (deal-list-into-two-hands movements)
    (hash-table-count (visit-houses santa-route (visit-houses robot-route)))))

(defun part2 (input) (share-route-with-robosanta input))

(5am:def-test part2 (:suite :aoc-2015-03)
  (5am:is (= 3 (part2 (parse-line "^v"))))
  (5am:is (= 3 (part2 (parse-line "^>v<"))))
  (5am:is (= 11 (part2 (parse-line "^v^v^v^v^v"))))

  (5am:is (= 2631 (part2 +input+))))
