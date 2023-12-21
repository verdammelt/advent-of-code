(defpackage #:aoc-2023-19
  (:use :cl))

(in-package #:aoc-2023-19)

(aoc:def-today-suite*)

(defun parse-workflow-clause-test (test-str)
  (let ((variable (char test-str 0))
        (test (string (char test-str 1)))
        (value (parse-integer test-str :start 2)))
    (list (find-symbol test)
          (find-symbol (format nil "~:@(part-~C~)" variable))
          value)))

(defun parse-workflow-clause (clause)
  (let ((parts (aoc:split-string-on-char #\: clause)))
    (if (= (length parts) 2)
        (list (parse-workflow-clause-test (first parts))
              (string-upcase (second parts)))
        (list t (string-upcase (first parts))))))

(defun parse-workflow (workflow-str)
  (destructuring-bind (name &rest clauses)
      (aoc:split-string-on-chars '(#\{ #\} #\,) workflow-str)
    (list (string-upcase name)
          (mapcar #'parse-workflow-clause clauses))))

(defun parse-part (part-str)
  (destructuring-bind (x xv m mv a av s sv)
      (aoc:split-string-on-chars '(#\{ #\} #\= #\,) part-str)
    (declare (ignore x m a s))
    (list :x (parse-integer xv)
          :m (parse-integer mv)
          :a (parse-integer av)
          :s (parse-integer sv))))

(defun parse-workflow-and-parts (lines)
  (destructuring-bind (workflows parts) (aoc:split-lines-on-empty-line lines)
    (list (mapcar #'parse-workflow workflows)
          (mapcar #'parse-part parts))))

(defun part-x (part) (getf part :x))
(defun part-m (part) (getf part :m))
(defun part-a (part) (getf part :a))
(defun part-s (part) (getf part :s))

(defun read-data (file)
  (aoc:read-data file :post-process #'parse-workflow-and-parts))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun checked-find-symbol (name)
  (let ((symbol (find-symbol name (find-package :aoc-2023-19-workflows))))
    (unless symbol (error "no symbol found with name ~S" name))
    symbol))

(defun sanitize-name (name)
  (let ((bad-names '(("NTH" . "ZZZZ"))))
    (if (assoc name bad-names :test #'string=)
        (cdr (assoc name bad-names))
        name)))

(defparameter *debug-workflow-calls* nil)

(defun compile-workflow (workflow)
  (destructuring-bind (name clauses) workflow
    (let* ((real-name (sanitize-name name))
           (symbol (intern (string-upcase real-name)
                          (find-package :aoc-2023-19-workflows))))
      (compile symbol
               `(lambda (part)
                  (when *debug-workflow-calls*
                    (format t "~&(~S ~S)" ,real-name part))
                  (cond ,@(loop for c in clauses
                               collect
                               (if (eq t (first c))
                                   `(t (funcall (checked-find-symbol
                                                 ,(sanitize-name (second c)))
                                                part))
                                   (destructuring-bind ((test variable-fn value) then) c
                                     `((,test (,variable-fn part) ,value)
                                       (funcall (checked-find-symbol ,(sanitize-name then))
                                                part))))))))
      (export symbol (find-package :aoc-2023-19-workflows))
      symbol)))

(defun make-terminal-function (name)
  (let ((symbol (intern (string-upcase name)
                        (find-package :aoc-2023-19-workflows))))
    (compile symbol `(lambda (part) (declare (ignore part)) ,(aoc:keywordize name)))
    (export symbol (find-package :aoc-2023-19-workflows))
    symbol))

(defun compile-workflows (workflows)
  (ignore-errors (delete-package :aoc-2023-19-workflows))
  (make-package :aoc-2023-19-workflows
                :use '(:cl :aoc-2023-19))

  (make-terminal-function "A")
  (make-terminal-function "R")


  (loop for work in workflows
        do (compile-workflow work))

  (find-symbol "IN" (find-package :aoc-2023-19-workflows)))

(defun part-sum (part)
  (+ (part-x part)
     (part-m part)
     (part-a part)
     (part-s part)))

(defun part1 (input)
  (destructuring-bind (workflows parts) input
    (let ((start-fn (compile-workflows workflows)))
      (aoc:sum
       (mapcar #'(lambda (p result)
                   (if (eq result :a)
                       (part-sum p)
                       0))
               parts
               (mapcar #'(lambda (p) (funcall start-fn p)) parts))))))b

(5am:def-test part1 (:suite :aoc-2023-19)
  (5am:is (= 19114 (part1 +example+)))
  (5am:is (= 420739 (part1 +input+))))

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2023-19)
  (5am:skip ":aoc-2023-19.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
