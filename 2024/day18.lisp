(defpackage #:aoc-2024-18
  (:use :cl))

(in-package #:aoc-2024-18)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data
   file
   :line-parser
   #'(lambda (str) (aoc:string-of-numbers->list-of-numbers str :delimiters `(#\,))) ))

(defparameter +example+
  (list :dimensions '(7 7)
        :falling-bytes (read-data (aoc:today-data-pathname "example"))))

(defparameter +input+
  (list :dimensions '(71 71)
        :falling-bytes (read-data (aoc:today-data-pathname))))

(defun find-path (ram start end)
  (aoc:dijkstra ram start end
                #'(lambda (graph node)
                    (remove-if-not
                     #'(lambda (pos) (gethash pos graph))
                     (mapcar #'(lambda (dir) (aoc:coord-add node dir))
                             (list (aoc:make-coord -1 0)
                                   (aoc:make-coord 1 0)
                                   (aoc:make-coord 0 -1)
                                   (aoc:make-coord 0 1))))
                    )
                (constantly 1)
                #'(lambda (graph) (alexandria:hash-table-keys graph))))

(defun corrupt-memory (ram pos)
  (remhash pos ram))

(defun setup-ram (input num)
  (let ((ram (make-hash-table :test #'equalp))
        (dimensions (getf input :dimensions))
        (falling-bytes (getf input :falling-bytes)))

    ;; all memory good to start
    (loop for x below (first dimensions)
          do (loop for y below (second dimensions)
                   do (setf (gethash (list x y) ram) #\.)))


    ;; corrupt some memory
    (dolist (pos (subseq falling-bytes 0 num))
      (corrupt-memory ram pos))

    ram))

(defun part1 (input num-fallen)
  (let ((ram (setup-ram input num-fallen))
        (start (aoc:make-coord 0 0))
        (end (apply #'aoc:make-coord (mapcar #'1- (getf input :dimensions)))))

    ;; find the shortest path to the exit
    (nth-value 0 (find-path ram start end))))

(5am:def-test part1 (:suite :aoc-2024-18)
  (5am:is (= 22 (part1 +example+ 12)))
  (5am:is (= 354 (part1 +input+ 1024))))

(defun part2 (input initial-num-fallen)
  (let ((ram (setup-ram input initial-num-fallen))
        (start (list 0 0))
        (end (apply #'list (mapcar #'1- (getf input :dimensions))))
        (falling-bytes (getf input :falling-bytes))
        (num-falling-bytes (length (getf input :falling-bytes))))

    (do ((i (1+ initial-num-fallen) (incf i))
         (path (nth-value 1 (find-path ram start end)))
         (get-path-count 1))
        ((or (not path) (>= i num-falling-bytes))
         (when (not path)
           (values (format nil "~{~D,~D~}" (nth (1- i) falling-bytes)) (1- i) get-path-count)))

      ;; (format t " ~D" i) (finish-output) (when (zerop (mod i 20)) (fresh-line))

      (let ((next-byte (nth i falling-bytes)))
        (corrupt-memory ram next-byte)
        (when (member next-byte path :test #'equalp)
          ;; (format t "*")
          (incf get-path-count)
          (setf path (nth-value 1 (find-path ram start end))))))))

(5am:def-test part2 (:suite :aoc-2024-18)
  (5am:is (string= "6,1" (part2 +example+ 12)))
  (5am:is (string= "36,17" (part2 +input+ 1024))))
