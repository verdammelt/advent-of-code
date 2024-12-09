(defpackage #:aoc-2024-09
  (:use :cl))

(in-package #:aoc-2024-09)

(aoc:def-today-suite*)

(defun read-data (file)
  (aoc:read-data file
                 :line-parser #'aoc:number-string->list-of-digits
                 :post-process #'first))

(defparameter +example+
  (read-data (aoc:today-data-pathname "example")))

(defparameter +input+
  (read-data (aoc:today-data-pathname)))

(defun file-idx (file-info) (car file-info))
(defun file-start (file-info) (caadr file-info))
(defun (setf file-start) (new-value file-info) (setf (caadr file-info) new-value))
(defun file-len (file-info) (cadadr file-info))
(defun (setf file-len) (new-value file-info) (setf (cadadr file-info) new-value))
(defun file-end (file-info) (+ (file-start file-info) (file-len file-info)))

(defun parse-disk-map (map)
  (do ((file-idx 0 (incf file-idx))
       (file-size (car map) (car rest))
       (space-size (cadr map) (cadr rest))
       (rest (cddr map) (cddr rest))
       (image (make-array (aoc:sum map) :initial-element nil))
       (image-idx 0)
       (disk-spaces (list))
       (disk-files (list)))
      ((null file-size) (values image (nreverse disk-spaces) (nreverse disk-files)))

    (when (plusp file-size)
      (push (list file-idx (list image-idx file-size)) disk-files))

    (dotimes (i file-size)
      (setf (aref image (+ i image-idx)) file-idx))
    (incf image-idx file-size)
    (when (and space-size (plusp space-size))
      (push (list nil (list image-idx space-size)) disk-spaces)
      (incf image-idx space-size))))

(defun frag (image)
  (do ((empty-idx 0)
       (full-idx (1- (length image))))
      ((> empty-idx full-idx) image)
    ;; (format t "~D=~s ~D=~S~&" empty-idx (aref image empty-idx) full-idx (aref image full-idx))
    (cond ((null (aref image full-idx)) (decf full-idx))
          ((not (null (aref image empty-idx))) (incf empty-idx))
          (t (setf (aref image empty-idx) (aref image full-idx)
                   (aref image full-idx) nil)))))

(defun checksum (image)
  (do ((sum 0 (incf sum (* idx (or (aref image idx) 0))))
       (idx 0 (incf idx)))
      ((= idx (length image)) sum)))

(defun part1 (input)
  (let ((disk-image (parse-disk-map input)))
    (checksum (frag disk-image))))

(5am:def-test part1 (:suite :aoc-2024-09)
  (5am:is (= 1928 (part1 +example+)))
  (5am:is (= 6446899523367 (part1 +input+))))

(defun defrag (disk-image spaces-list files-list)
  (dolist (file-to-try (reverse files-list) disk-image)
    (let ((idx-space-that-fits
            (position-if
             ;; find a space that is big enough and to the LEFT of the file to move
             #'(lambda (sp) (and (>= (file-len sp) (file-len file-to-try))
                            (< (file-start sp) (file-start file-to-try))))
             spaces-list)))
      ;; (format t "FILE: ~S SPACE: ~S=~S~&" file-to-try
      ;;         idx-space-that-fits (when idx-space-that-fits (nth idx-space-that-fits spaces-list)))

      (when idx-space-that-fits
        (let ((space (nth idx-space-that-fits spaces-list)))
          ;; copy the file from where it is to the space
          (setf (subseq disk-image (file-start space) (+ (file-start space) (file-len file-to-try)))
                (subseq disk-image (file-start file-to-try) (file-end file-to-try)))

          ;; overwrite the file area with spaces
          (setf (subseq disk-image (file-start file-to-try) (file-end file-to-try))
                (make-array (file-len file-to-try) :initial-element nil))

          ;; move the space to be after the new file and reduce its space to what is left
          (setf (file-start space) (+ (file-start space) (file-len file-to-try))
                (file-len space) (- (file-len space) (file-len file-to-try))))))))

(defun part2 (input)
  (checksum (multiple-value-call #'defrag (parse-disk-map input))))

(5am:def-test part2 (:suite :aoc-2024-09)
  (5am:is (= 2858 (part2 +example+)))
  (5am:is (= 132 (part2 '(1 2 3 4 5)))) ;; nothing should move!
  (5am:is (equalp (parse-disk-map '(1 2 3 4 5)) ;; nothing should move!
                   (multiple-value-call #'defrag (parse-disk-map '(1 2 3 4 5)))))
  (5am:is (= 6478232739671 (part2 +input+))))
