(defpackage #:aoc-2022-07
  (:use :cl))

(in-package #:aoc-2022-07)

(aoc:def-today-suite*)

(defclass log-line ()
  ((type :initarg :type :reader log-type)
   (data :initarg :data :reader log-data)))

(defclass cd-log (log-line)
  ((type :initform :cd)))
(defclass ls-log (log-line)
  ((type :initform :ls)))
(defclass dir-log (log-line)
  ((type :initform :dir)))
(defclass file-log (log-line)
  ((type :initform :file)))

(defmethod print-object ((obj log-line) stream)
  (print-unreadable-object (obj stream)
    (format stream "~A: ~A" (log-type obj) (log-data obj))))
(defmethod print-object ((obj ls-log) stream)
  (print-unreadable-object (obj stream)
    (format stream "~A" (log-type obj))))

(defmethod initialize-instance :after ((obj file-log) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (let ((data (slot-value obj 'data)))
    (setf (slot-value obj 'data)
          (list (second data) (parse-integer (first data))))))

(defun parse-log-line (str)
  (let ((parts (aoc:split-string-on-char #\Space str)))
    (flet ((command-p () (string= "$" (first parts)))
           (directory-p () (string= "dir" (first parts))))
      (cond ((and (command-p) (string= "cd" (second parts)))
             (make-instance 'cd-log :data (third parts)))
            ((and (command-p) (string= "ls" (second parts)))
             (make-instance 'ls-log))
            ((directory-p)
             (make-instance 'dir-log :data (second parts)))
            (t (make-instance 'file-log :data parts))))))

(defun read-data (file) (aoc:read-data file :line-parser #'parse-log-line))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun make-node (key value) (cons key value))
(defun make-dir (dir) (make-node dir nil))
(defun add-dir (node dir) (push (make-dir dir) node))
(defun add-file (node file)
  (if (assoc :files node)
      (push file (cdr (assoc :files node)))
      (push (cons :files (list file)) node))
  node)
(defun make-tree (root) (list (make-dir root)))
(defun get-node (tree path)
  (cond ((null tree) nil)
        ((null path) nil)
        ((null (rest path)) (cdr (assoc (first path) tree :test #'equal)))
        (t (get-node (cdr (assoc (first path) tree :test #'equal)) (rest path)))))

(defun (setf get-node) (new-value tree path)
  (cond ((null tree) (error "tree is null!"))
        ((null path) (error "path is null!"))
        ((null (rest path)) (setf (cdr (assoc (first path) tree :test #'equal)) new-value))
        (t (setf (get-node (cdr (assoc (first path) tree :test #'equal)) (rest path))
                 new-value))))

(defun modify-node (tree path fn)
  "Replaces node at PATH in TREE with the value FN on that node."
  (let ((current-node (get-node tree path)))
    (setf (get-node tree path) (funcall fn current-node))))

(defclass file-system ()
  ((cwd :initform (list "/") :accessor cwd)
   (file-tree :initform (make-tree "/")
              :reader file-tree)))

(defun make-file-system () (make-instance 'file-system))

(defvar *print-file-tree* nil)
(defmethod print-object ((obj file-system) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "CWD: ~A " (cwd obj))
    (when *print-file-tree*
      (format stream "~&TREE: ~A " (file-tree obj)))))

(defgeneric process-log-line (file-system log-line))
(defmethod process-log-line :around (file-system log-line)
  "Wrap processing a log line to ensure the FILE-SYSTEM argument is returned"
  (call-next-method file-system log-line)
  file-system)
(defmethod process-log-line (file-system (log-line ls-log))
  "Process a LS-LOG - nothing to do")
(defmethod process-log-line (file-system (log-line cd-log))
  "Process a CD-LOG - add the directory to the CWD (or pop one if dir is '..'"
  (cond ((string= ".." (log-data log-line))
         (pop (cwd file-system)))
        ((string= "/" (log-data log-line)) (setf (cwd file-system) (list "/")))
        (t
         (push (log-data log-line) (cwd file-system)))))
(defmethod process-log-line (file-system (log-line dir-log))
  "Process a DIR-LOG - add an empty DIR to our tree"
  (modify-node (file-tree file-system) (reverse (cwd file-system))
               #'(lambda (node) (add-dir node (log-data log-line)))))

(defmethod process-log-line (file-system (log-line file-log))
  "Process a FILE-LOG - add the file to our tree"
  (modify-node (file-tree file-system) (reverse (cwd file-system))
               #'(lambda (node) (add-file node (log-data log-line)))))

(defmethod process-log-line (file-system log-line)
  (format *error-output* "Unknown Command: ~A~&" log-line))

(defun process-log-lines (lines)
  (reduce #'process-log-line lines :initial-value (make-file-system)))

(defun directory-size (node)
  "A directory's size is the sum of all the files in the directory recursively."
  (cond ((null node) 0)
        ((eq :files (first node)) (reduce #'+ (cdr node) :key #'second))
        ((stringp (first node)) (directory-size (cdr node)))
        (t (reduce #'+ (mapcar #'directory-size node)))))

(defun directories (tree) (remove :files tree :key #'first))

(defun sizes-of-directories (nodes sizes)
  (let* ((dirs (directories nodes))
         (this-dir (first dirs)))
    (if (null this-dir) sizes
        (sizes-of-directories (append (rest dirs) (cdr this-dir))
                              (append (list (list (first this-dir)
                                                  (directory-size this-dir)))
                                      sizes)))))

(defun part1 (input)
  (let ((file-system (process-log-lines input)))
    (aoc:sum
     (mapcar #'second
             (remove-if #'(lambda (n) (> n 100000))
                        (remove "/"
                                (sizes-of-directories (file-tree file-system) (list))
                                :key #'first
                                :test #'string=)
                        :key #'second)))))

(5am:def-test part1 (:suite :aoc-2022-07)
  (5am:is (= 95437 (part1 +example+)))
  (5am:is (= 1306611 (part1 +input+))))

(defun part2 (input)
  (let* ((file-system (process-log-lines input))
         (needed-free-space 30000000)
         (sizes (sort (sizes-of-directories (file-tree file-system) (list)) #'< :key #'second))
         (current-available (- 70000000 (second (assoc "/" sizes :test #'string=))))
         (smallest-but-big-enough
           (find-if #'(lambda (size) (<= needed-free-space (+ (second size) current-available)))
                    sizes)))
    (second smallest-but-big-enough)))

(5am:def-test part2 (:suite :aoc-2022-07)
  (5am:is (= 24933642 (part2 +example+)))
  (5am:is (= 13210366 (part2 +input+))))
