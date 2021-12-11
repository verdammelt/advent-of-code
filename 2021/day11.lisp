(defpackage #:aoc-2021-11
  (:use :cl))

(in-package #:aoc-2021-11)

(aoc:def-today-suite*)

;; TODO: promote to utility (also in day09 with differnt name)
(defun number-string->list-of-digits (str)
  (mapcar #'digit-char-p (coerce str 'list)))

;; TODO: promote to utility (also in day09 with differnt name)
(defun lists->array (lists)
  (make-array (list (length lists) (length (first lists)))
              :initial-contents lists))

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'number-string->list-of-digits
                 :post-process #'lists->array))

(defun read-step-output (type step)
  (read-data (aoc:today-data-pathname (format nil "~A-step-~3,'0D" type step))))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun get-neighbors-coords (x y)
  (list
   ;; nw, n, ne
   (list (1- x) (1- y))
   (list x      (1- y))
   (list (1+ x) (1- y))
   ;; w, e
   (list (1- x) y)
   (list (1+ x) y)

   ;; sw, s, se
   (list (1- x) (1+ y))
   (list x      (1+ y))
   (list (1+ x) (1+ y))))

(defun filter-invalid-coords (array coords)
  (destructuring-bind (max-y max-x) (array-dimensions array)
    (remove-if #'(lambda (coord) (destructuring-bind (x y) coord
                              (or (minusp x)
                                  (minusp y)
                                  (>= x max-x)
                                  (>= y max-y))))
               coords)))

(defun get-valid-neighbors (array x y)
  (filter-invalid-coords array (get-neighbors-coords x y)))

(defun increase-all-energy (octopi-energy n)
  (let ((new (alexandria:copy-array octopi-energy)))
    (loop for i below (array-total-size new)
          do (incf (row-major-aref new i) n))
    new))

(defun perform-flash (octopi-energy)
  (let ((new (alexandria:copy-array octopi-energy)))
    (loop for i below (array-total-size new)
          when (> (row-major-aref new i) 9)
            do (setf (row-major-aref new i) 0))
    new))

(defun will-flash-p (array x y)
  (> (aref array y x) 9))

(defun all-possible-flashes (array)
  (loop for y below (array-dimension array 0)
        append (loop for x below (array-dimension array 1)
                     when (will-flash-p array x y) collect (list x y))))

(defun increase-energy-of-coords (energy coords)
  "Increase the energy of the given coordinates. This *mutates* ENERGY."
  (map 'nil #'(lambda (c) (incf (apply #'aref energy (reverse c)))) coords)
  energy)

(defun recursively-setup-flashes (energy will-flash have-flashed)
  (flet ((not-already-seen (coords)
           (remove-if #'(lambda (c) (member c (append will-flash have-flashed) :test #'equal)) coords))
         (ready-to-flash (coords)
           (remove-if-not #'(lambda (c) (apply #'will-flash-p energy c)) coords)))
    (let ((all-new-flashes (not-already-seen (all-possible-flashes energy))))
      (cond ((and (null will-flash) (null all-new-flashes)) energy)
            ((and (null will-flash) all-new-flashes)
             (recursively-setup-flashes energy all-new-flashes have-flashed))
            (t
             (let* ((neighbors (apply #'get-valid-neighbors energy (car will-flash))))
               ;; increment all neighbors of item that will flash
               (increase-energy-of-coords energy neighbors)

               ;; add new-neighbors that are ready to flash to will-flash list, add
               ;; this item to have-flashed list.
               (recursively-setup-flashes energy
                                          (append (cdr will-flash)
                                                  (ready-to-flash (not-already-seen neighbors)))
                                          (push (car will-flash) have-flashed))))))))


(defun setup-flashes (octopi-energy)
  (recursively-setup-flashes (alexandria:copy-array octopi-energy)
                             (all-possible-flashes octopi-energy)
                             (list)))

(defun do-step (octopi-energy)
  "Performs a single STEP and returns the new energy state"
  (perform-flash (setup-flashes (increase-all-energy octopi-energy 1))))

(defun do-n-steps (octopi-energy n)
  (loop for i from 1 to n
        for new-step = (do-step octopi-energy) then (do-step new-step)
        collecting new-step))

(defun count-flashes (octopi-energy)
  "Counts the 0 energy states which mean that a flash happened"
  (loop for i below (array-total-size octopi-energy)
        count (zerop (row-major-aref octopi-energy i))))

(defun part1 (input &optional (num-steps 100))
  (let ((steps (do-n-steps input num-steps)))
    (aoc:sum (mapcar #'count-flashes steps))))

(5am:def-test smaller-steps (:suite :aoc-2021-11)
  (let* ((smaller-data (read-data (aoc:today-data-pathname "smaller")))
         (smaller-step-1 (read-step-output "smaller" 1))
         (smaller-step-2 (read-step-output "smaller" 2))
         (steps (do-n-steps smaller-data 2)))
    (5am:is (equalp smaller-step-1 (nth 0 steps)))
    (5am:is (equalp smaller-step-2 (nth 1 steps)))))

(5am:def-test example-steps (:suite :aoc-2021-11)
  (let ((steps (do-n-steps +example+ 100)))
    (flet ((test-step-n (n)
             (5am:is (equalp (read-step-output "example" n) (nth (1- n) steps)))))
      (test-step-n 1)
      (test-step-n 2)
      ;; can add more steps here as needed - steps 1-10 and by tens steps 20-100
      ;; output are all available
      )))

(5am:def-test part1 (:suite :aoc-2021-11)
  (5am:is (= 204 (part1 +example+ 10)))
  (5am:is (= 1656 (part1 +example+)))
  (5am:is (= 1601 (part1 +input+))))

(defun all-flash-p (octopi)
  (= (count-flashes octopi) (array-total-size octopi)))

(defun part2 (input)
  (loop for x from 0
        for step = input then (do-step step)
        when (all-flash-p step) return x))

(5am:def-test part2 (:suite :aoc-2021-11)
  (5am:is (= 195 (part2 +example+)))
  (5am:is (= 368 (part2 +input+))))
