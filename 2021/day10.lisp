(defpackage #:aoc-2021-10
  (:use :cl))

(in-package #:aoc-2021-10)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defparameter +open-close-pairs+
  '((#\( . #\)) (#\[ . #\]) (#\{ . #\}) (#\< . #\>)))

(defun open-char-p (c) (not (null (assoc c +open-close-pairs+))))
(defun close-char-p (c) (not (null (rassoc c +open-close-pairs+))))

(defun closer-for-char (o) (cdr (assoc o +open-close-pairs+)))

(defun closer-for-char-p (o c)
  (char= c (closer-for-char o)))

(defparameter +syntax-error-scores+
  '((#\) . 3) (#\] . 57) (#\} . 1197) (#\> . 25137)))

(defun syntax-error-score (c)
  (cdr (assoc c +syntax-error-scores+)))

(defun parse-navigation-stream (stream opener-stack chunks)
  (let* ((next-char (read-char stream nil 'eof))
         (end-of-stream (eq next-char 'eof)))
    (cond ((and end-of-stream (not (null opener-stack)))
           (list :incomplete opener-stack))
          (end-of-stream
           (list :done chunks))

          ((open-char-p next-char)
           (parse-navigation-stream stream (push next-char opener-stack) chunks))

          ((and (close-char-p next-char)
                (closer-for-char-p (first opener-stack) next-char))
           (let ((opener (car opener-stack)))
             (parse-navigation-stream stream
                                      (cdr opener-stack)
                                      (push (list opener next-char) chunks))))

          ((close-char-p next-char)
           (list :corrupt next-char))

          (t (error "unknown character ~W" next-char)))))

(defun navigation-syntax-checker (line)
  (with-input-from-string (s line)
    (pairlis '(:input :result)
             (list line (parse-navigation-stream s (list) (list))))))

(defun corrupt-result-p (checker-output)
  (eq (cadr (assoc :result checker-output))
      :corrupt))

(defun incomplete-result-p (checker-output)
  (eq (cadr (assoc :result checker-output))
      :incomplete))

(defun corrupt-char (checker-output)
  (caddr (assoc :result checker-output)))

(defun incomplete-left-overs (checker-output)
  (caddr (assoc :result checker-output)))

(defun sum (nums) (reduce #'+ nums))

(defun part1 (input)
  (sum
   (mapcar #'syntax-error-score
           (mapcar #'corrupt-char
                   (remove-if-not #'corrupt-result-p
                                  (mapcar #'navigation-syntax-checker input))))))

(5am:def-test part1 (:suite :aoc-2021-10)
  (5am:is (= 26397 (part1 +example+)))
  (5am:is (= 339537 (part1 +input+))))

(defparameter +incomplete-error-scores+
  '((#\) . 1) (#\] . 2) (#\} . 3) (#\> . 4)))

(defun incomplete-error-score (c)
  (cdr (assoc c +incomplete-error-scores+)))

(defun score-incomplete-left-overs (left-overs)
  (reduce
   #'(lambda (score c)
       (+ (* score 5) (incomplete-error-score c)))
   left-overs
   :initial-value 0))

(defun part2 (input)
  (let ((scores
          (sort
           (mapcar #'score-incomplete-left-overs
                   (mapcar #'(lambda (left-overs) (mapcar #'closer-for-char left-overs))
                           (mapcar #'incomplete-left-overs
                                   (remove-if-not #'incomplete-result-p
                                                  (mapcar #'navigation-syntax-checker input)))))
           #'<)))
    (nth (floor (/ (length scores) 2)) (sort scores #'<))))

(5am:def-test part2 (:suite :aoc-2021-10)
  (5am:is (= 288957 (part2 +example+)))
  (5am:is (= 2412013412 (part2 +input+))))
