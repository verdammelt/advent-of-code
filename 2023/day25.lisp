(defpackage #:aoc-2023-25
  (:use :cl))

(in-package #:aoc-2023-25)

(aoc:def-today-suite*)

(defun parse-node (node-line)
  (destructuring-bind (node other-nodes) node-line
    (mapcar #'(lambda (n) (list (aoc:keywordize node) (aoc:keywordize n)))
            (aoc:split-string-on-char #\Space other-nodes))))

(defun parse-nodes (lines)
  (aoc:flatten
   (mapcar #'parse-node lines)))

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'(lambda (str) (aoc:split-string-on-char #\: str))
                 :post-process #'parse-nodes))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun write-graphviz-file (stream name nodes)
  (format stream "graph ~A {" name)
  (mapc #'(lambda (node-info)
            (destructuring-bind (node other-node) node-info
              (format stream "~&~A -- ~A;~%" node other-node)))
        nodes)
  (format stream "~%}"))

(defun make-cuts (graph cuts)
  (remove-if #'(lambda (n) (member n cuts
                              :test #'(lambda (l1 l2) (or (equalp l1 l2)
                                                     (equalp (reverse l1) l2)))))
             graph))

(defun neato (pathname)
  (uiop:run-program (format nil "neato -O -Tpng ~A" pathname))
  (format nil "~A.png" pathname))

(defun view-file (pathname)
  (uiop:run-program (list "open" (namestring pathname)))
  pathname)

(defun view-graph (name nodes)
  "Writes NODES as a graph named NAME to a Graphviz file. Then processes that file
and opens it for viewing. Returns the pathname of the Graphviz file (the
namestring of which will also contain NAME)."
  (flet ((write-nodes (stream)
           (write-graphviz-file stream name nodes))
         (view-nodes (pathname)
           (view-file (neato pathname))
           pathname))
    (uiop:call-with-temporary-file #'write-nodes
                                   :after #'view-nodes
                                   :keep t
                                   :type "gv"
                                   :suffix name
                                   :want-pathname-p nil)))

(defun get-node-counts (pathname)
  "Uses ccomps to determine the node counts from the provided Graphviz file at
PATHNAME."
  (let ((counts (nth-value 1
                           (uiop:run-program (list "ccomps" "-sv" (namestring pathname))
                                   :ignore-error-status t
                                  :ouput nil :error-output :lines))))
    (butlast
     (mapcar #'parse-integer
             (mapcar #'(lambda (str) (third (aoc:split-string-on-char #\Space str))) counts)))))

;;;
;;; this is manual solution:
;;;
;;; 1. use WRITE-GRAPHIZ-FILE to write the nodes to a graphiz file
;;; 2. `neato -O -Tpdf FILE.GV && open FILE.gv.pdf`
;;; 3. visually find the 3 cuts to make
;;; 4. use MAKE-CUTS to remove those 3 edges
;;; 5. use WRITE-GRAPHIZ-FILE to write the updated graph
;;; 5a. (optional) use neato again to double check the cuts were correct
;;; 6. `ccomp -sv` to print out the count of nodes in the disconnected graphs
;;;
;;; answer for input is 568214
(defun part1 (input)
  (view-graph "before" input)

  (format *query-io* "~& input the 3 cuts a list of pairs: ")
  (let* ((cuts (let ((*package* (find-package :keyword))) (read *query-io*)))
         (input-after-cuts (make-cuts input cuts)))
    (let ((pathname
            (view-graph "after" input-after-cuts)))
      (when (yes-or-no-p "Does the graph now show two separate nodes?")
        (aoc:product (get-node-counts pathname))))))
