(in-package #:aoc-2015)

(defstruct (point (:constructor point (x y)))
  (x) (y))

(defun point-add (p1 p2)
  (point (+ (point-x p1) (point-x p2))
         (+ (point-y p1) (point-y p2))))

(defun delivery-parse (str) (coerce str 'list))

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

(let ((test-data '((">" . 2)
                   ("^>v<" . 4)
                   ("^v^v^v^v^v" . 2))))
  (loop for (input . num-houses) in test-data
        do (assert (= (count-houses-visited (delivery-parse input))
                      num-houses))))

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

(let ((test-data '(("^v" . 3)
                   ("^>v<" . 3)
                   ("^v^v^v^v^v" . 11))))
  (loop for (input . num-houses) in test-data
        do (assert (= (share-route-with-robosanta (delivery-parse input))
                      num-houses))))
