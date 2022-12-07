(defpackage #:aoc-2022-07
  (:use :cl))

(in-package #:aoc-2022-07)

(aoc:def-today-suite*)

(defclass log-line ()
  ((type :initarg :type :reader log-type)
   (data :initarg :data :reader log-data)))
(defmethod print-object ((obj log-line) stream)
  (print-unreadable-object (obj stream)
    (format stream "~A: ~A" (log-type obj) (log-data obj))))

(defclass cd-log (log-line)
  ((type :initform :cd)))
(defclass ls-log (log-line)
  ((type :initform :ls)))
(defmethod print-object ((obj ls-log) stream)
  (print-unreadable-object (obj stream)
    (format stream "~A" (log-type obj))))
(defclass dir-log (log-line)
  ((type :initform :dir)))
(defclass file-log (log-line)
  ((type :initform :file)))
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

;;;
;;; FileSystem simulation
;;;
(defclass fs-node () ((name :initarg :name :reader name)))
(defmethod initialize-instance :after ((obj fs-node) &rest _args &key &allow-other-keys)
  (declare (ignore _args))
  (unless (slot-boundp obj 'name) (error "NAME must be provided when creating an FS-NODE")))
(defgeneric size (fs-node))

(defclass dir-node (fs-node) ((contents :accessor contents :initform nil)))
(defmethod print-object ((fs-node dir-node) stream)
  (print-unreadable-object (fs-node stream)
    (format stream "DIR: ~A: ~A" (name fs-node) (sort (copy-seq (contents fs-node)) #'node-less-than-p))))
(defun dir-node-p (fs-node) (typep fs-node 'dir-node))
(defgeneric subdirs (fs-node))
(defmethod subdirs ((fs-node dir-node))
  (remove-if-not #'dir-node-p (contents fs-node)))
(defun make-dir (name) (make-instance 'dir-node :name name))
(defun add-to-dir (dir node) (push node (contents dir)))
(defmethod size ((fs-node dir-node))
  (reduce #'+ (mapcar #'size (contents fs-node))))

(defclass file-node (fs-node) ((size :reader file-size :initarg :size)))
(defmethod print-object ((fs-node file-node) stream)
  (print-unreadable-object (fs-node stream)
    (format stream "FILE: ~A: ~A" (name fs-node) (size fs-node))))
(defmethod initialize-instance :after ((obj file-node) &rest _args &key &allow-other-keys)
  (declare (ignore _args))
  (unless (slot-boundp obj 'size) (error "FILE-SIZE must be provided when creating a FILE-NODE")))
(defun make-file (name size) (make-instance 'file-node :name name :size size))
(defmethod size ((fs-node file-node)) (file-size fs-node))

(defgeneric node-less-than-p (fs-node1 fs-node2))
(defmethod node-less-than-p ((fs-node1 fs-node) (fs-node2 fs-node))
  (string< (name fs-node1) (name fs-node2)))
(defmethod node-less-than-p ((fs-node1 dir-node) (fs-node2 file-node)) t)
(defmethod node-less-than-p ((fs-node1 file-node) (fs-node2 dir-node)) nil)

(defclass file-system ()
  ((cwd :initform (list "/") :accessor cwd)
   (file-tree :initform (make-dir "/") :reader file-tree)))
(defun make-file-system () (make-instance 'file-system))
(defmethod print-object ((obj file-system) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "CWD: ~A " (cwd obj))))

(defgeneric get-current-dir (file-system))
(defmethod get-current-dir (file-system)
  (let ((path (rest (reverse (cwd file-system))))) ;; skip "/"
    (reduce #'(lambda (n d) (find d (contents n) :key #'name :test #'string=)) path
            :initial-value (file-tree file-system))))

;;;
;;; Create the File System
;;;
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
  (add-to-dir (get-current-dir file-system)
              (make-dir (log-data log-line))))

(defmethod process-log-line (file-system (log-line file-log))
  "Process a FILE-LOG - add the file to our tree"
  (add-to-dir (get-current-dir file-system)
              (apply #'make-file (log-data log-line))))

(defmethod process-log-line (file-system log-line)
  (format *error-output* "Unknown Command: ~A~&" log-line))

(defun process-log-lines (lines)
  (reduce #'process-log-line lines :initial-value (make-file-system)))

(defun sizes-of-file-system (file-system)
  (labels ((recursive-sizes (dirs sizes)
             (let ((this-dir (first dirs)))
               (if (null this-dir) sizes
                   (recursive-sizes (append (rest dirs) (subdirs this-dir))
                                        (append (list (list (name this-dir) (size this-dir)))
                                                sizes))))))
    (recursive-sizes (list (file-tree file-system)) (list))))

(defun part1 (input)
  (aoc:sum
   (mapcar #'second
           (remove-if #'(lambda (n) (> n 100000))
                      (sizes-of-file-system (process-log-lines input)) :key #'second))))

(5am:def-test part1 (:suite :aoc-2022-07)
  (5am:is (= 95437 (part1 +example+)))
  (5am:is (= 1306611 (part1 +input+))))

(defun part2 (input)
  (let* ((needed-free-space 30000000)
         (sizes (sort (sizes-of-file-system (process-log-lines input)) #'< :key #'second))
         (current-available (- 70000000 (second (assoc "/" sizes :test #'string=))))
         (smallest-but-big-enough
           (find-if #'(lambda (size) (<= needed-free-space (+ (second size) current-available)))
                    sizes)))
    (second smallest-but-big-enough)))

(5am:def-test part2 (:suite :aoc-2022-07)
  (5am:is (= 24933642 (part2 +example+)))
  (5am:is (= 13210366 (part2 +input+))))
