(defpackage #:aoc-2020-19
  (:use :cl #:aoc #:aoc-2020/utils))

(in-package #:aoc-2020-19)

(aoc:def-today-suite*)

(defun parse-rule (rule)
  (destructuring-bind (num rule)
      (split-sequence:split-sequence #\: rule)
    (let ((num (parse-integer num))
          (rule (string-trim '(#\Space) rule)))
      (destructuring-bind (choice-a &optional choice-b)
          (split-sequence:split-sequence #\| rule)
        (flet ((parse-rule-list (liststr)
                 (read-from-string (format nil "(~A)" liststr))))
          (cond (choice-b
                 (cons num (list :or (parse-rule-list choice-a) (parse-rule-list choice-b))))
                ((char= #\" (char choice-a 0))
                 (cons num (char choice-a 1)))
                (t
                 (cons num (parse-rule-list choice-a)))))))))

(defun parse-message (message) (coerce message 'list))

(defun read-today-data (&optional label)
  (destructuring-bind (rules messages)
      (read-data (today-data-pathname label) :post-process #'split-on-empty-line)
    (list (mapcar #'parse-rule rules)
          (mapcar #'parse-message messages))))

(defun data-rules (data) (first data))
(defun data-messages (data) (second data))

(defun lookup-rule (rule-num rules) (cdr (assoc rule-num rules)))

(defparameter +example+ (read-today-data "example"))
(defparameter +example2+ (read-today-data "example2"))
(defparameter +input+ (read-today-data))

(defun apply-rule-list (rule-nums input rules)
  (reduce #'(lambda (in num) (apply-rule num in rules)) rule-nums :initial-value input))

(defun apply-rule (rule-num input rules)
  "Applies the RULE to the INPUT. returns left-over input, if no match found returns 'no-match"
  (let ((rule (lookup-rule rule-num rules)))
    (cond ((eq input 'no-match) 'no-match)
          ((null input) input)

          ;; single character match rule
          ((characterp rule) (if (char= rule (car input)) (cdr input)
                                 'no-match))


          ;; OR rule
          ((eq (car rule) :or)
           (let ((try-left (apply-rule-list (second rule) input rules)))
             (if (eq try-left 'no-match)
                 (apply-rule-list (third rule) input rules)
                 try-left)))

          ;; 'normal' rule of 1+ other rules to apply.
          (t (apply-rule-list rule input rules)))))

(defun check-rule (rule-num input rules)
  "If a RULE applies cleanly (no left-over) then return T, else NIL."
  (null (apply-rule rule-num input rules)))

(defun part1 (input &optional (target-rule 0))
  (let ((rules (data-rules input))
        (messages (data-messages input)))
    (count-if #'(lambda (m) (check-rule target-rule m rules)) messages)))

(5am:def-test part1 (:suite :aoc-2020-19)
  (5am:is (= 2 (part1 +example+)))
  (5am:is (= 3 (part1 +example2+)))
  (5am:is (= 180 (part1 +input+))))

(defparameter +new-rules+ '((8  . (:or (42) (42 8)))
                            (11 . (:or (42 31) (42 11 31)))))

(defun update-rules (updates data)
  (let ((rules (data-rules data))
        (messages (data-messages data)))
    (list (append updates rules) messages)))

(defun part2 (input &optional (target-rule 0))
  (part1 (update-rules +new-rules+ input) target-rule))

(5am:def-test part2 (:suite :aoc-2020-19)
  (5am:is (= 13 (part2 +example2+))))
