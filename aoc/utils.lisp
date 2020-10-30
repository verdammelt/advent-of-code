(in-package #:aoc)

(defun read-data (file)
  (with-open-file (stream file :direction :input)
    (uiop:slurp-stream-string stream)))

(defun read-data-lines (file)
  (with-open-file (stream file :direction :input)
    (uiop:slurp-stream-lines stream)))

(defun day-data-file (year day)
  (make-pathname :directory (list :relative (format nil "~D/inputs" year))
                 :name (format nil "day-~D" day)
                 :type "dat"))

(defun parsed-day-data (year day parser)
  (funcall parser (read-data-lines (day-data-file year day))))

(defun perform-day-task (year day task &optional (parser #'identity))
  (let ((result (funcall task (parsed-day-data year day parser))))
    (format t ";; Performing task ~S for Day ~D-~2,'0D... " task year day)
    (format t "~S~&" result)
    result))
