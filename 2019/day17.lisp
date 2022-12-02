(defpackage :flare
  (:use :common-lisp))

(in-package :flare)

(defun split-on (char) (lambda (str) (aoc:split-string-on-char char str)))

(defun load-program (file)
  (mapcar #'parse-integer
          (funcall (split-on #\,)
           (first (aoc:read-data file)))))

(defun make-map (output)
  (let* ((raw-data (map 'vector #'code-char
                        (mapcar #'parse-integer
                                (funcall (split-on #\Space)
                                         (string-trim '(#\Space) output)))))
         (num-lines (1- (count #\Newline raw-data)))
         (line-length (/ (1- (length raw-data)) num-lines)))
    (make-array (list num-lines line-length) :displaced-to raw-data)))

(defun alignment-parameters (program)
  (let* ((computer (computer:make-computer program
                                          (make-string-input-stream "")
                                          (make-string-output-stream)))
         (output (computer:get-output (computer:run-program computer))))
    (make-map output)))

(defun print-map (map &optional (stream nil))
  (format stream "~{~A~%~}"
          (mapcar #'(lambda (cs) (concatenate 'string cs))
                  (destructuring-bind (row col) (array-dimensions map)
                    (loop for r below row
                          collect (loop for c below col collect (aref map r c)))))))

;;; incomplete.

;; TODO: complete 2019.17
