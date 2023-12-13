(defpackage #:aoc-2023-12
  (:use :cl))

(in-package #:aoc-2023-12)

(aoc:def-today-suite*)

(defun parse-row (line)
  (destructuring-bind (condition-data group-sizes)
      (aoc:split-string-on-char #\Space line)
    (list condition-data
          (aoc:string-of-numbers->list-of-numbers group-sizes
                                                  :delimiters '(#\,)))))

(defconstant +ok+ #\.)
(defconstant +damaged+ #\#)
(defconstant +unknown+ #\?)

(defun row-conditions (row) (first row))
(defun row-groups (row) (second row))

(define-condition invalid-spring-condition (error)
  ((data :initarg :data :reader invalid-spring-condition-data)))

(defun spring-condition (conditions idx)
  (char conditions idx))
(defun (setf spring-condition) (new-value conditions idx)
  (setf (char conditions idx) new-value))
(defun spring-is-damaged (conditions idx)
  (if (member (spring-condition conditions idx) (list +damaged+ +unknown+))
      (setf (spring-condition conditions idx) +damaged+)
      (error 'invalid-spring-condition :data (list  conditions idx +damaged+))))
(defun spring-is-ok (conditions idx)
  (if (member (spring-condition conditions idx) (list +ok+ +unknown+))
      (setf (spring-condition conditions idx) +ok+)
      (error 'invalid-spring-condition :data (list conditions idx +ok+))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-row))

(defparameter +example+
  (mapcar #'parse-row '(
                        "???.### 1,1,3"
                        ".??..??...?##. 1,1,3"
                        "?#?#?#?#?#?#?#? 1,3,1,6"
                        "????.#...#... 4,1,1"
                        "????.######..#####. 1,6,5"
                        "?###???????? 3,2,1"
                        )))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

;;;
;;; could not get this solution working... trying to only compute valid possibilities
;;;
;; (defun all-positions (item seq &key (start 0) (end (length seq)) (test #'eql))
;;   (loop for i from start below end
;;         for x across seq
;;         when (funcall test item x) collect i))

;; (defun compute-possibilities (row)
;;   (let ((conditions (row-conditions row))
;;         (groups (row-groups row))
;;         (possibilities (list)))
;;     (dolist (start (all-positions +unknown+ conditions))
;;       (let ((this-try (substitute +ok+ +unknown+ conditions :end start)))
;;         (format t "~& working on: ~S " this-try)

;;         (handler-case
;;             (progn (let ((idx start))
;;                      (dolist (size groups)
;;                        (setf idx (position-if #'(lambda (c) (member c (list +damaged+ +unknown+))) this-try :start idx))
;;                        (unless idx (error 'invalid-spring-condition :data "nothing to replace"))
;;                        (dotimes (i size)
;;                          (spring-is-damaged this-try (+ idx i)))
;;                        (unless (= (length this-try) (+ idx size))
;;                          (spring-is-ok this-try (+ idx size)))
;;                        (incf idx size)
;;                        (format t "~&TRY: ~S" this-try)))
;;                    (push this-try possibilities))
;;           (invalid-spring-condition (c)
;;             (format t "INVALID CONFIGURATION SEEN - ~S~&"
;;                     (invalid-spring-condition-data c))))))
;;     possibilities))

;;;
;;; Brute force method of computing *ALL* possibilities and then finding those that are valid
;;;
(defun compute-all-possibilities (in-progress possibilities)
  (cond ((null in-progress) possibilities)
        ((not (position +unknown+ (first in-progress)))
         (compute-all-possibilities
          (rest in-progress)
          (push (first in-progress) possibilities)))
        (t (let ((idx (position +unknown+ (first in-progress))))
             (let ((copy (copy-seq (first in-progress))))
               (setf (spring-condition (first in-progress) idx) +ok+
                     (spring-condition copy idx) +damaged+)
               (compute-all-possibilities (push copy in-progress) possibilities))))))

(defun valid-configuration (sizes configuration)
  (let* ((damaged-sections (aoc:split-string-on-char +ok+ configuration))
         (lengths (mapcar #'length damaged-sections)))
    (equal lengths sizes)))

(defun find-valid-possibilities (possibilities sizes)
  (remove-if-not #'(lambda (configuration) (valid-configuration sizes configuration)) possibilities))

(defun part1 (input)
  (reduce #'(lambda (count poss) (+ count (length poss)))
          (mapcar
           #'(lambda (row) (let ((conditions (row-conditions row))
                            (sizes (row-groups row)))
                        (find-valid-possibilities (compute-all-possibilities (list (copy-seq conditions)) (list))
                                                  sizes)))
           input)
          :initial-value 0))

(5am:def-test part1 (:suite :aoc-2023-12)
  (5am:is (= 21 (part1 +example+)))
  (5am:is (= 7753 (part1 +input+))))

(defun unfold (row)
  (let ((conditions (make-list 5 :initial-element (row-conditions row)))
        (groups (make-list 5 :initial-element (row-groups row))))
    (list (format nil "~{~A~^?~}" conditions)
          (apply #'append groups))))

(defun part2 (input)
  ;; baaaad idea
  ;; (part1 (mapcar #'unfold input))
  )

(5am:def-test part2 (:suite :aoc-2023-12)
  (5am:skip ":aoc-2023-12.2 not implemented")
  ;; (5am:is (= 525152 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
