(defpackage #:aoc-2019-08
  (:use :cl))

(in-package #:aoc-2019-08)

(aoc:def-today-suite*)

(defun split-into-layers (dimensions data)
  (let* ((per-layer (apply #'* dimensions))
         (num-layers (/ (length data) per-layer)))
    (loop :for i :below num-layers
       :collect (subseq data (* i per-layer) (* (1+ i) per-layer)))))

(defun read-image-data (file)
  (first (aoc:read-data file)))

(defun checksum (dimensions file)
  (let* ((data (read-image-data file))
         (layers (split-into-layers dimensions data))
         (layer-with-fewest-zeros
          (first (sort layers #'< :key #'(lambda (str) (count #\0 str))))))
    (* (count #\1 layer-with-fewest-zeros)
       (count #\2 layer-with-fewest-zeros))))

(defun apply-layer-onto (current new)
  (flet ((apply-pixel (a b) (if (char= a #\2) b a)))
    (map 'string #'apply-pixel current new)))

(defun flatten-layers (layers)
  (reduce #'apply-layer-onto layers
          :initial-value (make-string (length (first layers))
                                      :initial-element #\2)))

(defun layer->image (layer dimensions)
  (let ((num-cols (first dimensions))
        (num-rows (second dimensions)))
    (loop :for i :below num-rows
         :collect (subseq layer (* i num-cols) (* (1+ i) num-cols)))))

(defun color-image (image color-map)
  (flet ((replace-char (c) (cdr (assoc c color-map :test #'char=))))
    (mapcar #'(lambda (row) (map 'string #'replace-char row)) image)))

(defun render-data (dimensions data)
  (let* ((layers (split-into-layers dimensions data))
         (final-layer (flatten-layers layers))
         (image (layer->image final-layer dimensions))
         (colored-image (color-image image '((#\1 . #\#) (#\0 . #\Space)))))
    (format nil "~&~{~A~%~}" colored-image)))

(defun render (dimensions file)
  (render-data dimensions (read-image-data file)))

(5am:def-test part1 (:suite :aoc-2019-08)
  (5am:is (= 1452 (checksum '(25 6) (aoc:today-data-pathname)))))

(5am:def-test part2 (:suite :aoc-2019-08)
  (5am:is (string=
           (format nil "~{~A~%~}"
                   '("###  #  # ###  #### #  # "
                     "#  # #  # #  # #    #  # "
                     "#  # #### #  # ###  #  # "
                     "###  #  # ###  #    #  # "
                     "#    #  # #    #    #  # "
                     "#    #  # #    ####  ##  "))

           (render '(25 6) (aoc:today-data-pathname)))))
