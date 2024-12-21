(defpackage #:aoc-2024-21
  (:use :cl))

(in-package #:aoc-2024-21)

(aoc:def-today-suite*)

(defun read-data (file) (aoc:read-data file))

(defun neighbors (graph node)
  (mapcar #'first (gethash node graph)))

(defun cost (graph node1 node2)
  (declare (ignore graph node1 node2))
  1)

(defun all-vertexes (graph)
  (alexandria:hash-table-keys graph))

(defun find-paths-on-pad (graph start)
  (aoc:dijkstra graph start nil #'neighbors #'cost #'all-vertexes))

(defun path->directions (pad path)
  (do* ((path path (cdr path))
        (first-key (car path) (car path))
        (second-key (cadr path) (cadr path))
        (directions (list)))
       ((null second-key) (reverse directions))
    (push (second (assoc second-key (gethash first-key pad)))
          directions)))

(defun all-paths-on-pad (pad)
  (let ((paths (make-hash-table :test #'equalp)))
    (dolist (key (all-vertexes pad) paths)
      (multiple-value-bind (costs paths-from-key)
          (find-paths-on-pad pad key)
        (declare (ignore costs))
        (maphash #'(lambda (key2 path) (setf (gethash (list key key2) paths) (reverse path)))
                 paths-from-key)))))

(defparameter +number-pad+
  (let ((graph (make-hash-table :test #'equalp)))
    (setf (gethash #\7 graph) '((#\4 #\v) (#\8 #\>))
          (gethash #\8 graph) '((#\7 #\<) (#\9 #\>) (#\5 #\v))
          (gethash #\9 graph) '((#\8 #\<) (#\6 #\v))
          (gethash #\4 graph) '((#\7 #\^) (#\5 #\>) (#\1 #\v))
          (gethash #\5 graph) '((#\4 #\<) (#\6 #\>) (#\8 #\^) (#\2 #\v))
          (gethash #\6 graph) '((#\5 #\<) (#\9 #\^) (#\3 #\v))
          (gethash #\1 graph) '((#\4 #\^) (#\2 #\>))
          (gethash #\2 graph) '((#\1 #\<) (#\3 #\>) (#\5 #\^) (#\0 #\v))
          (gethash #\3 graph) '((#\2 #\<) (#\6 #\^) (#\A #\v))
          (gethash #\0 graph) '((#\2 #\^) (#\A #\>))
          (gethash #\A graph) '((#\3 #\^) (#\0 #\<)))
    graph))

(defparameter +number-pad-all-paths+ (all-paths-on-pad +number-pad+))

(defparameter +direction-pad+
  (let ((graph (make-hash-table :test #'equalp)))
    (setf (gethash #\^ graph) '((#\A #\>) (#\v #\v))
          (gethash #\A graph) '((#\^ #\<) (#\> #\v))
          (gethash #\< graph) '((#\v #\>))
          (gethash #\v graph) '((#\< #\<) (#\> #\>) (#\^ #\^))
          (gethash #\> graph) '((#\v #\<) (#\A #\^)))
    graph))

(defparameter +direction-pad-all-paths+ (all-paths-on-pad +direction-pad+))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun find-path-for-sequence (pad-paths sequence)
  (do* ((curr-key #\A next-key)
        (seq (coerce sequence 'list) (cdr seq))
        (next-key (car seq) (car seq))
        (path (list)))
       ((null seq) (nreverse path))
    (push (gethash (list curr-key next-key) pad-paths) path)))

(defun paths->string (paths)
  (format nil "~{~A~}"
          (mapcar #'(lambda (p) (concatenate 'string (append p '(#\A)))) paths)))

(defun sequence->directions (sequence pad pad-all-paths)
  (paths->string
   (mapcar #'(lambda (path) (path->directions pad path))
           (find-path-for-sequence pad-all-paths sequence))))

(defun directions-for-sequence-on-number-pad (sequence)
  (sequence->directions sequence +number-pad+ +number-pad-all-paths+))

(defun directions-for-sequence-on-direction-path (sequence)
  (sequence->directions sequence +direction-pad+ +direction-pad-all-paths+))

(defun keys-for-sequnce-with-n-robots (sequence n)
  (do ((keys (directions-for-sequence-on-number-pad sequence)
             (directions-for-sequence-on-direction-path keys))
       (num-robots 1 (incf num-robots)))
      ((= num-robots n) keys)))

(defun part1 (input)
  (mapcar #'(lambda (seq) (keys-for-sequnce-with-n-robots seq 2)) input))

(5am:def-test part1 (:suite :aoc-2024-21)
  (5am:skip ":aoc-2024-21.1 not complete - keypad sequences are WAY TOO LONG")
  ;; (5am:is (= -1 (part1 +example+)))
  ;; (5am:is (= -1 (part1 +input+)))
  )

(defun part2 (input) (declare (ignore input)) 0)

(5am:def-test part2 (:suite :aoc-2024-21)
    (5am:skip ":aoc-2024-21.2 not implemented")
  ;; (5am:is (= -1 (part2 +example+)))
  ;; (5am:is (= -1 (part2 +input+)))
  )
