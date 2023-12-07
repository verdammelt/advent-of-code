(defpackage #:aoc-2020-02
  (:use :cl))

(in-package #:aoc-2020-02)

(aoc:def-today-suite*)

(defun parse-password-info (line)
  (destructuring-bind (range character password)
      (aoc:split-string-on-char #\Space line)
    (list password
          (list (aoc:string-of-numbers->list-of-numbers range #\-)
                (char character 0)))))

(defparameter +data-set+
  (aoc:read-data (aoc:today-data-pathname) :line-parser #'parse-password-info))
(defparameter +short-data-set+
  (aoc:read-data (aoc:today-data-pathname "example") :line-parser #'parse-password-info))

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

(5am:test part1
  (5am:is (= 2 (first (find-valid-passwords +short-data-set+ #'sled-rental-password-validity-checker))))
  (5am:is (= 422 (first (find-valid-passwords +data-set+ #'sled-rental-password-validity-checker)))))

(5am:test part2
  (5am:is (= 1 (first (find-valid-passwords +short-data-set+ #'corporate-password-validity-checker))))
  (5am:is (= 451 (first (find-valid-passwords +data-set+ #'corporate-password-validity-checker)))))
