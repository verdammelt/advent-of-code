(defpackage :day5
  (:use :common-lisp))

(in-package :day5)

;; (load "../computer")

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

(defparameter *program-input* (read-input (file-utils:file-in-day "./input.txt" 5)))

(defun day5-part1 ()
  (first
   (remove-if #'zerop
              (mapcar #'parse-integer
                      (string-utils:split
                       (string-trim '(#\Space)
                                    (with-output-to-string (output)
                                      (with-input-from-string (input "1")
                                        (computer:compute *program-input* :input-stream input
                                                                          :output-stream output))))
                       #\Space)))))

(assert (= 7988899 (day5-part1)))

(defun day5-part2 ()
  (parse-integer
   (string-trim '(#\Space)
                (with-output-to-string (output)
                  (with-input-from-string (input "5")
                    (computer:compute *program-input*
                                      :input-stream input
                                      :output-stream output))))))

(assert (= 13758663 (day5-part2)))
