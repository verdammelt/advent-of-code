(defpackage #:aoc-2020/test
  (:use :cl :5am)
  (:shadow :run-all-tests)
  (:export :run-all-tests))

(in-package #:aoc-2020/test)

(defun run-all-tests (&key (on-error nil))
  (let ((results (run 'all-tests :print-names t)))
    (if (and (eq on-error :signal)
             (find-if #'5am::test-failure-p results))
        (error "Test Failures")
        results)))

(def-suite all-tests)
(in-suite all-tests)

(test dummy-test
  (is (eq t t))
  (is (eq 0 1)))
