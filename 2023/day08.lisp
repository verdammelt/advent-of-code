(defpackage #:aoc-2023-08
  (:use :cl))

(in-package #:aoc-2023-08)

(aoc:def-today-suite*)

(defun find-tokens (str)
  (aoc:split-string-on-chars '(#\Space #\= #\( #\) #\,) str))

(defun parse-directions (str)
  (mapcar #'aoc:keywordize (coerce str 'list)))

(defun parse-node (data) data)

(defun node-name-equal (n1 n2) (string= n1 n2))
(defun node-name (node) (first node))

(defun find-node (nodes name)
  (find name nodes :key #'node-name :test #'node-name-equal))

(defun traverse-node (node direction)
  (if (eq direction :L)
      (second node)
      (third node)))

(defun map-directions (map) (first map))
(defun map-nodes (map) (second map))

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'find-tokens
                 :post-process #'aoc:split-lines-on-empty-line))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +example-2+
  (read-data (aoc:today-data-pathname "example-2")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun parse-map (raw-map)
  (destructuring-bind (((directions)) (&rest nodes)) raw-map
    (list (parse-directions directions)
          (mapcar #'parse-node nodes))))

(defun count-steps (map start finished-p)
  (let ((nodes (map-nodes map)))
    (labels ((count-steps-iter (current directions count)
               (cond ((funcall finished-p current) (values count current directions ))
                     (t (count-steps-iter
                         (traverse-node (find-node nodes current) (first directions))
                         (or (rest directions) (map-directions map))
                         (1+ count))))))
      (count-steps-iter start (map-directions map) 0))))

(defun part1 (input)
  (count-steps (parse-map input) "AAA" #'(lambda (node) (node-name-equal node "ZZZ"))))

(5am:def-test part1 (:suite :aoc-2023-08)
  (5am:is (= 2 (part1 +example+)))
  (5am:is (= 6 (part1 +example-2+)))
  (5am:is (= 17621 (part1 +input+))))

(defun string-ends-with (str suffix)
  (string= (reverse str) suffix :end1 (length suffix)))

(defun nodes-ending-in (map suffix)
  (remove-if-not
   #'(lambda (name) (string-ends-with name suffix))
   (mapcar #'node-name (map-nodes map))))

(defparameter +example-3+
  (read-data (aoc:today-data-pathname "example-3")))

(defun terminal-node-p (name)
  (string-ends-with name "Z"))

(defun count-steps-from-nodes-to-terminals (map start-nodes)
  (loop for node in start-nodes
        collect (count-steps map node #'terminal-node-p)))

(defun part2 (input)
  "Find all the starting nodes in the map and then count the steps from each to a
terminal node. Finally take LCM of those numbers. The paths from start to
terminal are cycles so we are looking for a place when all cycles line up."
  (let* ((map (parse-map input))
         (start-nodes (nodes-ending-in map "A")))
    (apply #'lcm (count-steps-from-nodes-to-terminals map start-nodes))))

(5am:def-test part2 (:suite :aoc-2023-08)
  (5am:is (= 2 (part2 +example+)))
  (5am:is (= 6 (part2 +example-2+)))
  (5am:is (= 6 (part2 +example-3+)))
  (5am:is (= 20685524831999 (part2 +input+))))
