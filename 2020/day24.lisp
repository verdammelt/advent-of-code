(defpackage #:aoc-2020-24
  (:use :cl #:aoc))

(in-package #:aoc-2020-24)

(5am:def-suite :aoc-2020-24 :in :aoc-2020)

(defun parse-path (path)
  (with-input-from-string (stream (string-downcase path))
    (do ((parsed (list))
         (c (read-char stream nil nil) (read-char stream nil nil)))
        ((not c) (nreverse parsed))
      (push (ecase c
               (#\e :east)
               (#\w :west)
               (#\s (let ((p (read-char stream)))
                      (ecase p
                        (#\e :southeast)
                        (#\w :southwest))))
               (#\n (let ((p (read-char stream)))
                      (ecase p
                        (#\e :northeast)
                        (#\w :northwest)))))
            parsed))))

(defun read-paths (&optional label)
  (read-data (today-data-pathname label) :line-parser #'parse-path))

(defparameter +example+ (read-paths "example"))
(defparameter +input+ (read-paths))

(defun make-point (x y) (complex x y))
(defun point-x (point) (realpart point))
(defun point-y (point) (imagpart point))

(defparameter +directions+ '((:east . #C (2 0))
                             (:west . #C (-2 0))
                             (:northwest . #C (-1 1))
                             (:northeast . #C (1 1))
                             (:southwest . #C (-1 -1))
                             (:southeast . #C (1 -1))))

(defun follow-path (path)
  (reduce #'(lambda (point direction) (+ point (cdr (assoc  direction +directions+))))
          path
          :initial-value #C (0 0)))

(5am:def-test follow-path (:suite :aoc-2020-24)
  (5am:is (= #C(0 0) (follow-path (parse-path "nwwswee")))))

(defun toggle-tile (black-tiles path)
  (let ((tile (follow-path path)))
    (multiple-value-bind (value foundp) (gethash tile black-tiles)
      (declare (ignore value))
      (if foundp (remhash tile black-tiles)
          (setf (gethash tile black-tiles) t))))
  black-tiles)

(defun count-black-tiles (black-tiles)
  (hash-table-count black-tiles))

(defun part1 (input)
  (count-black-tiles (reduce #'toggle-tile input :initial-value (make-hash-table))))

(5am:def-test part1 (:suite :aoc-2020-24)
  (5am:is (= 10 (part1 +example+)))
  (5am:is (= 326 (part1 +input+))))
