(in-package :aoc-tests)
(5am:in-suite aoc-tests)

(5am:test rosetta-example
  (let ((graph '(((:a :b) . 7)
                 ((:a :c) . 9)
                 ((:a :f) . 14)
                 ((:b :c) . 10)
                 ((:b :d) . 15)
                 ((:c :d) . 11)
                 ((:c :f) . 2)
                 ((:d :e) . 6)
                 ((:e :f) . 9))))
    (flet ((neighbors-of (edges v)
             (apply #'append (mapcar #'cdar (remove-if-not #'(lambda (edge) (eq (caar edge) v)) edges))))
           (edge-cost (edges u v)
             (let ((cost (cdr (assoc (list u v) edges :test #'equalp))))
               (if cost cost (error "No edge between ~A and ~A found" u v))))
           (vertices-from-edges (edges)
             (remove-duplicates (reduce #'append (mapcar #'first edges)))))

      (multiple-value-bind (dist path)
          (aoc:dijkstra graph :a :e #'neighbors-of #'edge-cost #'vertices-from-edges)
        (5am:is (equal 26 dist))
        (5am:is (equal '(:e :d :c :a) path))))))
