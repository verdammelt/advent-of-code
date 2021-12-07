(in-package #:aoc-2015)

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

(assert (= 280 (perform-day-task 2015 1 #'what-floor)))
(assert (= 1797 (perform-day-task 2015 1 #'when-floor)))

(assert (= 1588178 (perform-day-task 2015 2 #'total-square-feet #'parse-to-dimensions)))
(assert (= 3783758 (perform-day-task 2015 2 #'total-ribbon-size #'parse-to-dimensions)))

(assert (= 2572 (perform-day-task 2015 3 #'count-houses-visited #'delivery-parse)))
(assert (= 2631 (perform-day-task 2015 3 #'share-route-with-robosanta #'delivery-parse)))

(assert (= 117946 (perform-day-task 2015 4 #'first-advent-coin-idx #'parse-secret-key)))
(assert (= 3938038 (perform-day-task 2015 4 #'first-index-with-six-zeros #'parse-secret-key)))

(assert (= 255 (perform-day-task 2015 5 #'count-if-nice #'parse-list)))
(assert (= 55 (perform-day-task 2015 5 #'count-if-nice-2 #'parse-list)))
