(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../file-utils"))

(defpackage :space-image-format
  (:use :common-lisp)
  (:export :checksum :render))

(in-package :space-image-format)

(defun split-into-layers (dimensions data)
  (let* ((per-layer (apply #'* dimensions))
         (num-layers (/ (length data) per-layer)))
    (loop :for i :below num-layers
       :collect (subseq data (* i per-layer) (* (1+ i) per-layer)))))

(defun read-image-data (file)
  (first (file-utils:read-lines file)))

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
    (format t "~&~{~A~%~}" colored-image)))

(defun render (dimensions file)
  (render-data dimensions (read-image-data file)))

;; part 1 answer = 2413
;; part 2 answer:
;;
;; ###   ##  ###  #### ###
;; #  # #  # #  #    # #  #
;; ###  #    #  #   #  ###
;; #  # #    ###   #   #  #
;; #  # #  # #    #    #  #
;; ###   ##  #    #### ###
;;
