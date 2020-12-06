(defpackage #:aoc-2020-02
  (:use :cl))

(in-package #:aoc-2020-02)

(defun parse-password-info (line)
  (destructuring-bind (range character password)
      (split-sequence:split-sequence #\Space line)
    (list password
          (list (mapcar #'parse-integer (split-sequence:split-sequence #\- range))
                (char character 0)))))

(defparameter +data-set+
  (aoc:read-data (aoc:data-pathname "day2" "txt") :line-parser #'parse-password-info))
(defparameter +short-data-set+
  (aoc:read-data (aoc:data-pathname "day2-example" "txt") :line-parser #'parse-password-info))

(defun find-all (item sequence &key from-end (test #'eql) (start 0) end count key)
  (remove-if-not #'(lambda (x) (funcall test x item)) sequence
                 :from-end from-end :start start :end end :count count :key key))

(defun find-all-if (fn sequence &key from-end (start 0) end count key)
  (remove-if-not fn sequence
                 :from-end from-end :start start :end end :count count :key key))

(defun sled-rental-password-validity-checker (password rule)
  (let ((range (first rule))
        (character (second rule)))
    (<= (first range)
       (length (find-all character password))
       (second range))))

(defun corporate-password-validity-checker (password rule)
  (flet ((char-at-= (idx char str) (char= char (char str idx))))
    (let ((positions (mapcar #'1- (first rule)))
          (character (second rule)))
      (= 1 (count t (mapcar #'(lambda (idx) (char-at-= idx character password)) positions))))))

(defun find-valid-passwords (data validity-checker)
  (let ((valid-passwords (find-all-if #'(lambda (item) (apply validity-checker item)) data)))
    (list (length valid-passwords) valid-passwords)))
