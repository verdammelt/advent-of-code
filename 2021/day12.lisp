(defpackage #:aoc-2021-12
  (:use :cl))

(in-package #:aoc-2021-12)

(aoc:def-today-suite*)

(defun parse-node-line (line)
  (aoc:split-string-on-char #\- line))

(defun make-graph-from-nodes (nodes)
  (reduce #'(lambda (g n)
              (pushnew (second n) (gethash (first n) g (list)) :test #'string=)
              (pushnew (first n) (gethash (second n) g (list)) :test #'string=)
              g)
          nodes
          :initial-value (make-hash-table :test #'equal)))

(defun big-cave-p (c) (every #'upper-case-p c))
(defun small-cave-p (c) (every #'lower-case-p c))

(defparameter +first-cave+ "start")
(defparameter +last-cave+ "end")

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'parse-node-line
                 :post-process #'make-graph-from-nodes))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +example-2+
  (read-data (aoc:today-data-pathname "example-2")))

(defparameter +example-3+
  (read-data (aoc:today-data-pathname "example-3")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defvar *allowed-cave-rule* nil)

(defun allowed-cave-p (cave path)
  (if *allowed-cave-rule* (funcall *allowed-cave-rule* cave path)
      (error "~A is NIL" (symbol-name '*allowed-cave-rule*))))

(defun single-pass-small-cave-rule (cave path)
  (or (big-cave-p cave)
      (not (find cave path :test #'string=))))

(defun recursively-find-path (caves curr exits path paths)
  (labels ((at-end-p (curr) (string= curr +last-cave+))
           (allowed-p (exit) (allowed-cave-p exit path)))
    (let ((allowed-exits (remove-if-not #'allowed-p exits)))
      (cond ((at-end-p curr) (append (list path) paths))
            ((null allowed-exits) paths)
            (t
             (let ((new-paths
                     (recursively-find-path caves
                                            (car allowed-exits)
                                            (gethash (car allowed-exits) caves)
                                            (append (list (car allowed-exits)) path)
                                            paths)))
               (recursively-find-path caves
                                      curr
                                      (cdr allowed-exits)
                                      path
                                      new-paths)))))))

(defun find-paths (caves rule)
  (mapcar #'reverse
          (let ((*allowed-cave-rule* rule))
            (recursively-find-path caves
                                   +first-cave+
                                   (gethash +first-cave+ caves)
                                   (list +first-cave+)
                                   (list)))))

(defun part1 (input)
  (length (find-paths input #'single-pass-small-cave-rule)))

(5am:def-test part1 (:suite :aoc-2021-12)
  (5am:is (= 10 (part1 +example+)))
  (5am:is (= 19 (part1 +example-2+)))
  (5am:is (= 226 (part1 +example-3+)))
  (5am:is (= 4167 (part1 +input+))))

(defun item-counts (list &key (test #'eql))
  (let ((counts (list)))
    (dolist (item list)
      (if (assoc item counts :test test)
          (incf (cdr (assoc item counts :test test)))
          (setf counts (acons item 1 counts))))
    counts))

(defun single-small-cave-twice-rule (cave path)
  (let* ((small-caves-in-path (remove-if #'big-cave-p path))
         (caves-in-path-count (item-counts small-caves-in-path :test #'string=))
         (twice-cave (rassoc 2 caves-in-path-count))
         (is-start-or-end-p (member cave (list +first-cave+ +last-cave+) :test #'string=))
         (in-path-p (find cave path :test #'string=)))
    (cond
      ;; start and end only allowed if not yet seen.
      (is-start-or-end-p (not in-path-p))
      ;; big caves are always OK
      ((big-cave-p cave) t)
      ;; no cave seen twice yet - so any small cave fine.
      ((null twice-cave) t)
      ;; we've seen a small cave twice - this one is OK if we haven't seen it yet.
      (t (not in-path-p)))))

(defun part2 (input)
  (length (find-paths input #'single-small-cave-twice-rule)))

(5am:def-test part2 (:suite :aoc-2021-12)
  (5am:is (= 36 (part2 +example+)))
  (5am:is (= 103 (part2 +example-2+)))
  (5am:is (= 3509 (part2 +example-3+)))
  (5am:is (= 98441 (part2 +input+))))
