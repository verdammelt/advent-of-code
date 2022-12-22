(defpackage #:aoc-2022-21
  (:use :cl))

(in-package #:aoc-2022-21)

(aoc:def-today-suite*)

(defun make-operation (symbol)
  (ecase symbol
    (:+ #'+)
    (:- #'-)
    (:* #'*)
    (:/ #'/)))

(defun make-equation (num-or-monkey &optional operation monkey)
  (if (numberp num-or-monkey) num-or-monkey
      (list (make-operation operation) num-or-monkey monkey)))

(defun make-monkey (name &rest equation)
  (cons name (apply #'make-equation equation)))

(defun parse-monkey (str)
  (apply #'make-monkey
         (mapcar #'aoc:number-or-keyword
                 (aoc:split-string-on-chars '(#\: #\Space) str))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-monkey))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun get-monkey (monkies name)
  (assoc name monkies))

(defun monkey-has-value-p (monkey)
  (numberp (cdr monkey)))

(defun monkey-value (monkey)
  (and (monkey-has-value-p monkey) (cdr monkey)))

(defun compute-monkey (monkies monkey)
  (destructuring-bind (op m1 m2) (cdr monkey)
    (flet ((get-value (name) (monkey-value (get-monkey monkies name))))
      (when (and (get-value m1)
                 (get-value m2))
        (funcall op (get-value m1) (get-value m2))))))

(defun set-monkey-value (monkey value)
  (setf (cdr monkey) value))

(defun monkey-depedencies (monkies monkey)
  (when (not (monkey-has-value-p monkey))
    (mapcar #'(lambda (n) (get-monkey monkies n)) (cddr monkey))))

(defun compute-under-root (monkies)
  "Computes all values *under* the root monkey"
    (do ((queue (monkey-depedencies monkies (get-monkey monkies :root))))
        ((null queue) monkies)
      (let ((monkey (pop queue)))
        ;; (format t "Q: ~S M: ~S~&" queue monkey)
        (unless (monkey-has-value-p monkey)
          (let ((new-value (compute-monkey monkies monkey)))
            (if (numberp new-value)
                (set-monkey-value monkey new-value)
                (let ((monkey-depedencies (monkey-depedencies monkies monkey)))
                  (push monkey queue)
                  (push (second monkey-depedencies) queue)
                  (push (first monkey-depedencies) queue))))))))

(defun part1 (input)
  (let ((monkies (compute-under-root (copy-tree input))))
    (compute-monkey monkies (get-monkey monkies :root))))

(5am:def-test part1 (:suite :aoc-2022-21)
  (5am:is (= 152 (part1 +example+)))
  (5am:is (= 51928383302238 (part1 +input+))))

(defun root-diff-with-human-value (monkies human-value)
  (let* ((monkies (copy-tree monkies))
         (human (get-monkey monkies :humn))
         (root (get-monkey monkies :root))
         (root-dependencies (monkey-depedencies monkies root)))
    (setf (cdr human) human-value)
    (compute-under-root monkies)
    (apply #'- (mapcar #'monkey-value root-dependencies))))

(defun secant-method (fn v1 v2 done-p)
  "Repeatedly calls FN on value and refines those values (starting with V1 and V2)
until DONE-P returns non-NIL which will then be returned as the value of this
function.

FN is called with a single value.

DONE-p is called with V1, V2, FN(V1) and FN(V2)"
  (do ((answer nil))
      (answer answer)
    (let ((d1 (funcall fn v1))
          (d2 (funcall fn v2)))
      ;; (format t "v1: ~D v2: ~D d1: ~D d2: ~D~&" v1 v2 d1 d2)
      (psetf answer (funcall done-p v1 v2 d1 d2)
             v1 v2
             v2 (- v1 (/ (* (- v2 v1) d1) (- d2 d1)))))))

(defun part2 (input)
  (secant-method #'(lambda (v) (root-diff-with-human-value input v))
                 most-negative-fixnum most-positive-fixnum
                 #'(lambda (v1 v2 d1 d2)
                     (cond ((zerop d1) v1)
                           ((zerop d2) v2)
                           (t nil)))))

(5am:def-test part2 (:suite :aoc-2022-21)
  (5am:is (= 301 (part2 +example+)))
  (5am:is (= 3305669217840 (part2 +input+))))
