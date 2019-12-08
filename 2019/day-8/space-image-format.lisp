(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "../file-utils"))

(defpackage :space-image-format
  (:use :common-lisp)
  (:export :checksum))

(in-package :space-image-format)

(defun checksum (dimensions file)
  (let* ((data (first (file-utils:read-lines file)))
         (per-layer (apply #'* dimensions))
         (num-layers (/ (length data) per-layer))
         (layers (loop :for i :below num-layers
                    :collect (subseq data (* i per-layer) (* (1+ i) per-layer))))
         (layer-with-fewest-zeros
          (first (sort layers #'< :key #'(lambda (str) (count #\0 str))))))
    (* (count #\1 layer-with-fewest-zeros)
       (count #\2 layer-with-fewest-zeros))))

;; part 1 answer = 2413
