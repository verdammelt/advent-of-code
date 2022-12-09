(in-package :aoc)

#|
function Dijkstra(Graph, source):
  for each vertex v in Graph.Vertices:
    dist[v] ← INFINITY
    prev[v] ← UNDEFINED
    add v to Q
  dist[source] ← 0

  while Q is not empty:
    u ← vertex in Q with min dist[u]
    remove u from Q

    for each neighbor v of u still in Q:
      alt ← dist[u] + Graph.Edges(u, v)
      if alt < dist[v]:
        dist[v] ← alt
        prev[v] ← u

  return dist[], prev[]
|#

(defun vertices-from-edges (edges)
  (remove-duplicates (reduce #'append (mapcar #'first edges))))

(defun less-than (x y)
  "Less than handling possible NIL values. NIL == Infinity"
  (cond ((null y) t)
        ((null x) nil)
        (t (< x y))))

(defun make-costs () (list))
(defun cost-of (costs x)
  (cdr (assoc x costs)))

(defun make-queue (items) items)
(defun queue-empty-p (queue) (zerop (length queue)))
(defun pop-least (queue costs)
  (setf queue (sort queue #'less-than :key #'(lambda (x) (cost-of costs x))))
  (list (pop queue) queue))
(defun in-queue-p (queue item) (member item queue))

(defun neighbors-of (edges v)
  (apply #'append (mapcar #'cdar (remove-if-not #'(lambda (edge) (eq (caar edge) v)) edges))))
(defun edge-cost (edges u v)
  (let ((cost (cdr (assoc (list u v) edges :test #'equalp))))
    (if cost cost (error "No edge between ~A and ~A found" u v))))

(defun dijkstra (edges source)
  (let ((vertices (make-queue (vertices-from-edges edges)))
        (dist (make-costs))
        (prev (list)))
    (push (cons source 0) dist)

    (loop while (not (queue-empty-p vertices))
          for (u new-queue) = (pop-least vertices dist)
          do (setf vertices new-queue)

          do (loop for v in (remove-if-not #'(lambda (x) (in-queue-p vertices x))
                                           (neighbors-of edges u))
                   for alt = (+ (cost-of dist u) (edge-cost edges u v))
                   when (less-than alt (cost-of dist v))
                     do (progn (push (cons v alt) dist)
                               (push (cons v u) prev)))

          finally (return (values dist prev)))))
