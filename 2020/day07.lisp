(defpackage #:aoc-2020-07
  (:use :cl :aoc-2020/utils :aoc))

(in-package #:aoc-2020-07)

(defparameter +bag-regexp+ "(\\d*)[ ]*(\\w+ \\w+) bag")

(defun find-all-bags (input)
  (mapcar #'(lambda (m) (nth-value 1 (cl-ppcre:scan-to-strings +bag-regexp+ m)))
          (cl-ppcre:all-matches-as-strings +bag-regexp+ input)))

(defun make-bag (desc)
  (intern (string-upcase (substitute #\- #\Space desc)) :keyword))

(defun parse-bag-amount (desc)
  (let ((amount (elt desc 0))
        (bag (elt desc 1)))
    (if (string= "no other" bag) nil
        (list (make-bag bag) (parse-integer amount)))))

(defun parse-rule (input)
  (let* ((parsed-line (find-all-bags input))
         (container (make-bag (elt (first parsed-line) 1)))
         (contains (mapcar #'parse-bag-amount (rest parsed-line))))
    (cons container contains)))

(defparameter +input+ (read-data (today-data) :line-parser #'parse-rule))
(defparameter +example+ (read-data (today-data "example") :line-parser #'parse-rule))
(defparameter +example-2+ (read-data (today-data "example-2") :line-parser #'parse-rule))

(defun who-can-contain (bag rules)
  (labels ((can-contain-bag (bag)
             (mapcar #'car
                     (remove-if-not
                      #'(lambda (rule) (member bag (rest rule) :key #'car))
                      rules)))
           (containers (bags containers)
             (if (null bags) (remove-duplicates containers)
                 (let ((new-containers (can-contain-bag (first bags))))
                   (containers (append (cdr bags) new-containers)
                        (append containers new-containers))))))
    (containers (list bag) (list))))

(assert (equal (who-can-contain :shiny-gold +example+)
               '(:BRIGHT-WHITE :MUTED-YELLOW :LIGHT-RED :DARK-ORANGE)))

(defun repeat (item n) (loop for i below n collect item))

(defun required-contents (bag rules)
  (labels ((contents (bags contents)
             (if (null bags) contents
                 (let* ((in-this-bag (cdr (assoc (first bags) rules)))
                        (all-bags (mapcan #'(lambda (bag-and-num) (when bag-and-num (apply #'repeat bag-and-num)))
                                          in-this-bag)))
                   (contents (append (cdr bags) all-bags)
                             (append contents all-bags))))))
    (contents (list bag) (list))))

(assert (= 32 (length (required-contents :shiny-gold +example+))))
(assert (= 126 (length (required-contents :shiny-gold +example-2+))))
