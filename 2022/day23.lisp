(defpackage #:aoc-2022-23
  (:use :cl))

(in-package #:aoc-2022-23)

(aoc:def-today-suite*)

(defun parse-map (strs)
  (let ((map (make-hash-table :test #'equal)))
    (dotimes (row (length strs))
      (dotimes (col (length (first strs)))
        (when (char= #\# (char (nth row strs) col))
          (setf (gethash (list row col) map) t))))
    map))

(defun read-data (file) (aoc:read-data file :post-process #'parse-map))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun north-neighbors (coord)
  (destructuring-bind (row col) coord
    `((,(1- row) ,(1- col)) (,(1- row) 0) (,(1- row) ,(1+ col)))))

(defun south-neighbors (coord)
  (destructuring-bind (row col) coord
    `((,(1+ row) ,(1- col)) (,(1+ row) 0) (,(1+ row) ,(1+ col)))))

(defun west-neighbors (coord)
  (destructuring-bind (row col) coord
    `((,(1- row) ,(1- col)) (,row ,(1- col)) (,(1+ row) ,(1- col)))))

(defun east-neighbors (coord)
  (destructuring-bind (row col) coord
    `((,(1- row) ,(1+ col)) (,row ,(1+ col)) (,(1+ row) ,(1+ col)))))

(defun partial-apply-map (map-fn map)
  (lambda (&rest args) (apply map-fn map args)))

(defun elf-at-p (map coord)
  (gethash coord map))

(defun north-rule (map coord)
  (when (notany (partial-apply-map #'elf-at-p map) (north-neighbors coord))
    (list (1- (first coord)) (second coord))))

(defun south-rule (map coord)
  (when (notany (partial-apply-map #'elf-at-p map) (south-neighbors coord))
    (list (1+ (first coord)) (second coord))))

(defun west-rule (map coord)
  (when (notany (partial-apply-map #'elf-at-p map) (west-neighbors coord))
    (list (first coord) (1- (second coord)))))

(defun east-rule (map coord)
  (when (notany (partial-apply-map #'elf-at-p map) (east-neighbors coord))
    (list (first coord) (1+ (second coord)))))

(defun rotate-list (list)
  (let ((first (first list))
        (rest (rest list)))
    (append rest (list first))))

(defun get-map-bounds (map)
  (let ((min-row most-positive-fixnum)
        (min-col most-positive-fixnum)
        (max-row most-negative-fixnum)
        (max-col most-negative-fixnum))
   (maphash #'(lambda (key _value) (declare (ignore _value))
                (destructuring-bind (row col) key
                  (setf min-row (min min-row row)
                        max-row (max max-row row)
                        min-col (min min-col col)
                        max-col (max max-col col))))
            map)

    (list min-row min-col max-row max-col)))

(defun draw-map (map &optional (stream t))
  (destructuring-bind (min-row min-col max-row max-col)
      (get-map-bounds map)

    (format stream "(~D,~D) => (~D,~D)~%~%" min-row min-col max-row max-col)

    (do ((row min-row (1+ row)))
        ((> row max-row))
      (format stream " ")
      (do ((col min-col (1+ col)))
          ((> col max-col))
        (format stream "~C" (if (gethash (list row col) map) #\# #\.)))
      (format stream "~&"))

    (format stream "~%")))

(defun do-round (map rules)
  (let ((moves (make-hash-table :test #'equal)))
    (maphash #'(lambda (coord _value)
                 (declare (ignore _value))
                 (let ((new-coord (some #'(lambda (r) (funcall r map coord)) rules)))
                   (when new-coord
                     (format t "~S => ~S~&" coord new-coord)
                     (push coord (gethash new-coord moves (list))))))
             map)

    (let ((new-map (make-hash-table :test #'equal)))
      (maphash #'(lambda (to froms)
                   (if (= 1 (length froms))
                       ;; move the elf
                       (setf (gethash to new-map) t)
                       ;; more than one elf going to same place - so no moves
                       (dolist (f froms) (setf (gethash f new-map) t))))
               moves)
      new-map)
    ))

(defun part1 (input) (declare (ignore input)) 0)

;; TODO: complete 2022-23.1
(5am:def-test part1 (:suite :aoc-2022-23)
  (5am:skip ":aoc-2022-23.1 not implemented")
  ;; (5am:is (= -1 (part1 +example+)))
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

;; TODO: complete 2022-23.2
(5am:def-test part2 (:suite :aoc-2022-23)
  (5am:skip ":aoc-2022-23.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
