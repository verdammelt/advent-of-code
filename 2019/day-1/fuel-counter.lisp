(load "../file-utils")

(defpackage :fuel-counter
  (:use :common-lisp))

(in-package :fuel-counter)

(defun fuel-cost (mass)
  (- (floor mass 3) 2))

(defun total-fuel-cost (mass)
  (labels ((helper (m costs)
             (let ((cost (fuel-cost m)))
               (if (< cost 1) costs
                   (helper cost (append costs (list cost)))))))
    (reduce #'+ (helper mass (list)))))

(defun parse-line (line)
  (with-input-from-string (stream line) (read stream)))

(defun execute (input-file)
  (let* ((masses (mapcar #'parse-line (file-utils:read-lines input-file)))
         (fuel-costs (mapcar #'total-fuel-cost masses)))
    (reduce #'+ fuel-costs)))
