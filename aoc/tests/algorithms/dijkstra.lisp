(in-package :aoc-tests)
(5am:in-suite aoc-tests)

(5am:def-suite rosetta-example)

(defvar *graph*
  '(((:a :b) . 7)
    ((:a :c) . 9)
    ((:a :f) . 14)
    ((:b :c) . 10)
    ((:b :d) . 15)
    ((:c :d) . 11)
    ((:c :f) . 2)
    ((:d :e) . 6)
    ((:e :f) . 9)))

(defun neighbors-of (edges v)
  (aoc:flatten (mapcar #'cdar (remove-if-not #'(lambda (edge) (eq (caar edge) v)) edges))))

(defun edge-cost (edges u v)
  (let ((cost (cdr (assoc (list u v) edges :test #'equalp))))
    (if cost cost (error "No edge between ~A and ~A found" u v))))

(defun vertices-from-edges (edges)
  (remove-duplicates (reduce #'append (mapcar #'first edges))))

(defun hash-table->alist (hash-table)
  (let ((alist (list)))
    (maphash #'(lambda (key value) (push (cons key value) alist)) hash-table)
    alist))

(5am:test start-to-end
  (multiple-value-bind (dist path)
      (aoc:dijkstra *graph* :a :e #'neighbors-of #'edge-cost #'vertices-from-edges)
    (5am:is (equal 26 dist))
    (5am:is (equal '(:e :d :c :a) path))))

(5am:test start-to-all
  (multiple-value-bind (dists paths)
      (aoc:dijkstra *graph* :a nil #'neighbors-of #'edge-cost #'vertices-from-edges)
    (5am:is (equal '((:E . 26) (:D . 20) (:F . 11) (:C . 9) (:B . 7) (:A . 0))
                   (hash-table->alist dists)))
    (5am:is (equal '((:E . (:E :D :C :A)) (:D . (:D :C :A)) (:F . (:F :C :A))
                     (:C . (:C :A)) (:B . (:B :A)))
                   (hash-table->alist paths)))))
