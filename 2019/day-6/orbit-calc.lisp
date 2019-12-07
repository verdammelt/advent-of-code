(load "../file-utils")
(load "../string-utils")

(defpackage :orbit-calc
  (:use :common-lisp)
  (:export :checksum))

(in-package :orbit-calc)

;; orbit data is a hashtable of child -> parent
(defun add-orbit (orbits com sat)
  (setf (gethash sat orbits) com)
  orbits)

(defun make-initial-orbits ()
  (let ((orbits (make-hash-table :test #'equal)))
    (setf (gethash "COM" orbits) nil)
    orbits))

(defun orbiting (orbits sat)
  (gethash sat orbits))

(defun load-orbits (datafile)
  "LOAD-ORBITS reads the data file and returns a orbit representation"
  (labels ((split (s) (string-utils:split s #\)))
           (slurp (file) (mapcar #'split (file-utils:read-lines file))))
    (let* ((orbit-pairs (slurp datafile)))
      (reduce #'(lambda (orbits pair)
                  (add-orbit orbits (first pair) (second pair)))
              orbit-pairs
              :initial-value (make-initial-orbits)))))

(defun orbit-path (orbits sat)
  "Orbital 'path' from satellite SAT to center-of-masss COM"
  (labels ((path-to-com (obj path)
             (if (not (orbiting orbits obj)) (push obj path)
                 (path-to-com (orbiting orbits obj) (push obj path)))))
    (path-to-com sat (list))))

(defun orbit-count (orbits)
  (labels ((counter (obj count)
             (if (not (orbiting orbits obj)) count
                 (counter (orbiting orbits obj) (1+ count)))))
    (loop for key being the hash-key of orbits
       sum (counter key 0))))


(defun orbit-checksum (orbits)
  "CHECKSUM is calcuated by summing up the direct and indirect orbits in ORBITS"
  (orbit-count orbits))

(defun orbit-transfers (orbits from to)
  (let ((from-path (orbit-path orbits from))
        (to-path (orbit-path orbits to)))
    (append (set-difference from-path to-path :test #'equal)
            (nreverse (set-difference to-path from-path :test #'equal)))))

(defun num-orbit-transfers (orbits from to)
  (- (length (orbit-transfers orbits from to)) 2))
