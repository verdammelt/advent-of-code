(in-package :aoc)

;; TODO: expand to also be useful to get all paths with their distances from start.
(defun dijkstra (graph start end neighbors cost all-vertexes)
  "Find the shortest path between START and END on GRAPH.

NEIGHBORS is a function that takes the GRAPH and a node and returns a list of
nodes accessible on GRAPH from NODE.

COST is a function that takes GRAPH and two nodes and return the cost of
traveling between those nodes or NIL if there is no path.

ALL-VERTEXES is a function that takes GRAPH and returns a list of all nodes of
the GRAPH."
  (let ((queue (funcall all-vertexes graph))
        (dist (make-hash-table :test #'equalp))
        (prev (make-hash-table :test #'equalp)))
    (setf (gethash start dist) 0)
    (labels ((less-than (x y) (cond ((and x y) (< x y))
                                    ((null x) nil)
                                    (t t)))
             (pop-least ()
               (setf queue (sort queue #'less-than :key #'(lambda (node) (gethash node dist))))
               (pop queue))
             (path-to (node)
               (append (list node)
                       (loop for next = (gethash node prev) then (gethash next prev)
                             until (not next)
                             collect next)))
             (plus-costs (x y) (if (and x y) (+ x y) nil)))
      (loop while (plusp (length queue))
            for u = (pop-least)
            when (equalp u end)
              do (return (values (gethash u dist) (path-to u)))
            do (loop for v in (funcall neighbors graph u)
                     do (let ((alt (plus-costs (gethash u dist)
                                               (funcall cost graph u v))))
                          (when (less-than alt (gethash v dist))
                            (setf (gethash v dist) alt)
                            (setf (gethash v prev) u))))))))
