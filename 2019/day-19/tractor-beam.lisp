(defpackage #:tractor-beam
  (:use :common-lisp)
  (:export))

(in-package #:tractor-beam)

(defun check-xy (program x y)
  (with-output-to-string (output)
    (computer:compute
     program
     :input-stream (make-string-input-stream (format nil "~D ~D" x y))
     :output-stream output)))

(defun scan-area (file max-x max-y)
  (let* ((program (computer:load-program file))
         (map (make-array (list max-x max-y)
                          :element-type 'character
                          :initial-element #\?)))
    (dotimes (x max-x)
      (dotimes (y max-y)
        (let* ((at-spot (check-xy program x y))
               (map-value (if (= 1 (parse-integer at-spot)) #\# #\.)))
          (setf (aref map x y) map-value))))
    map))

;;; part 1 correc answer: 171
(defun count-beam-pulling (map)
  (loop for idx from 0 below (array-total-size map)
     count (char= (row-major-aref map idx) #\#)))

(defun print-map (map)
  (with-output-to-string (output)
    (loop for x from 0 below (array-dimension map 0)
       do (loop for y from 0 below (array-dimension map 1)
             do (format output "~C" (aref map x y))
             finally (terpri output)))))
