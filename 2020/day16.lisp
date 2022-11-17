(defpackage #:aoc-2020-16
  (:use :cl #:aoc #:aoc-2020/utils))

(in-package #:aoc-2020-16)

(aoc:def-today-suite*)

(defun parse-field-ranges (str)
  (cl-ppcre:register-groups-bind (field min1 max1 min2 max2)
      ("([\\w ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)" str)
    (list field
          (lambda (x)
            (or (<= (parse-integer min1) x (parse-integer max1))
                (<= (parse-integer min2) x (parse-integer max2))))
          str)))

(defun field-name (f) (first f))
(defun field-validator (f) (second f))
(defun field-description (f) (third f))

(defun parse-ticket (str)
  (mapcar #'parse-integer (aoc:split-string-on-char #\, str)))

(defun parse-input (lines)
  (destructuring-bind (field-ranges your-ticket nearby-tickets)
      (split-on-empty-line lines)
    (list (mapcar #'parse-field-ranges field-ranges)
          (parse-ticket (second your-ticket))
          (mapcar #'parse-ticket (cdr nearby-tickets)))))

(defparameter +input+ (read-data (today-data-pathname)
                                 :post-process #'parse-input))
(defparameter +example+ (read-data (today-data-pathname "example")
                                   :post-process #'parse-input))
(defparameter +example-2+ (read-data (today-data-pathname "example-2")
                                     :post-process #'parse-input))

(defun input-fields (input) (first input))
(defun input-your-ticket (input) (second input))
(defun input-nearby-tickets (input) (third input))


(defun value-valid-for-fields (fields)
  (lambda (value)
    (remove-if-not #'(lambda (f) (funcall (field-validator f) value)) fields)))

(defun ticket-invalid-values (fields ticket)
  (remove-if (value-valid-for-fields fields) ticket))

(defun part1 (input)
  (reduce #'+
          (flatten
           (mapcar (partial #'ticket-invalid-values (input-fields input))
                   (input-nearby-tickets input)))))

(5am:def-test part1 (:suite :aoc-2020-16)
  (5am:is (= 71 (part1 +example+)))
  (5am:is (= 21996 (part1 +input+))))

(defun valid-ticket-p (fields ticket)
  (null (ticket-invalid-values fields ticket)))

(defun possible-fields-for-ticket (ticket fields)
  (mapcar #'(lambda (ss) (mapcar #'first ss))
          (mapcar (value-valid-for-fields fields) ticket)))

(defun reduce-choices-for-field (choices)
  (reduce #'(lambda (acc next-set) (intersection acc next-set :test #'string=))
          choices))

(defun find-solution (initial-choices)
  (flet ((single-choice-p (c) (= 1 (length c)))
         (remove-choice (c cs) (remove c cs :test #'string=)))
    (do ((choices initial-choices)
         (solution (list)))
        ((every #'null choices) (sort solution #'< :key #'first))
      (let* ((single-choice-idx (position-if #'single-choice-p choices))
             (single-choice (first (nth single-choice-idx choices))))
        (setf solution (push (list single-choice-idx single-choice) solution)
              choices (mapcar (partial #'remove-choice single-choice) choices))))))

(defun starts-with-p (str prefix)
  (string= str prefix :end1 (min (length str) (length prefix))))

(defun part2 (input)
  (let* ((fields (input-fields input))

         (my-ticket (input-your-ticket input))

         (valid-nearby-tickets
           (remove-if-not (partial #'valid-ticket-p fields) (input-nearby-tickets input)))

         (possible-fields-for-tickets
          (mapcar (rpartial #'possible-fields-for-ticket fields) valid-nearby-tickets))

         (possible-fields-per-field (apply #'mapcar #'list possible-fields-for-tickets))

         (choices-for-fields (mapcar #'reduce-choices-for-field possible-fields-per-field))

         (solution (find-solution choices-for-fields))

         (departure-fields (remove-if-not (rpartial #'starts-with-p "departure")
                                          solution :key #'second))

         (field-indexes-to-pick (mapcar #'first departure-fields)))

    (reduce #'*
            (mapcar #'(lambda (idx) (nth idx my-ticket)) field-indexes-to-pick))))

(5am:def-test part2 (:suite :aoc-2020-16)
  (5am:is (= 650080463519 (part2 +input+))))
