(defpackage :day5
  (:use :common-lisp))

(load "../computer")

;;; from day 2
;;; TODO find place for this utility
(defun split-string (string &optional (delimeter #\Space))
  (labels ((split-string-iter (string result)
             (let ((idx (position delimeter string :from-end t)))
               (if idx
                   (split-string-iter (subseq string 0 idx)
                                      (cons (subseq string (1+ idx)) result))
                   (cons string result)))))
    (split-string-iter string (list))))

;;; TODO find place for this utility
(defun read-input (file)
  (with-open-file (stream file :direction :input)
    (mapcar #'parse-integer (split-string (read-line stream) #\,))))

(defparameter *program-input* (read-input "./input.txt"))

(defun day5-part1 ()
  "correct answer is: 7692125"
  (with-input-from-string (input "1")
    (computer:compute *program-input* :input-stream input)))

(defun day5-part2 ()
  "correct ansewr is: 14340395"
  (with-output-to-string (output)
    (with-input-from-string (input "5")
      (computer:compute *program-input*
                        :input-stream input
                        :output-stream output))))
