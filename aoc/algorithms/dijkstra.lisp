(in-package :aoc)

;; TODO: expand to also be useful to get all paths with their distances from start.
(defun dijkstra (graph start end neighbors cost all-vertexes)
  "Find the shortest path between START and END on GRAPH. If END is NIL then
find all shortest paths.

What this function returns differs depending on if END is NIL or not.

If END is not NIL the function returns two values 1) the distance from START to END
and 2) a list which is a path of nodes from START to END.

If END is NIL the function returns two values 1) a hash table mapping nodes to
their distance from START and 2) a hash-table mapping node to a list which is a
path of nodes from START to that node.

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
            when (equalp u end) do (return)
            do (loop for v in (funcall neighbors graph u)
                     do (let ((alt (plus-costs (gethash u dist)
                                               (funcall cost graph u v))))
                          (when (less-than alt (gethash v dist))
                            (setf (gethash v dist) alt)
                            (setf (gethash v prev) u)))))
      (if end
          (values (gethash end dist) (path-to end))
          (let ((paths (make-hash-table :test #'equalp)))
            (maphash #'(lambda (node prev-node)
                         (declare (ignore prev-node))
                         (setf (gethash node paths) (path-to node)))
                     prev)
            (values dist paths))))))
