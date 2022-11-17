(defpackage :aoc-2019/utils
  (:use :common-lisp)
  (:export :init-day :remove-day))

(in-package :aoc-2019/utils)

(defconstant +this-year+
  (nth 5 (multiple-value-list (decode-universal-time (get-universal-time)))))

(defun touch (pathname)
  (when (probe-file pathname) (error 'file-error :pathname pathname))
  (open pathname :direction :probe :if-does-not-exist :create))

(defun template-defsystem (dir system-name)
  (with-open-file (stream (merge-pathnames (format nil "~A.asd" system-name) dir)
                          :if-exists :error :direction :output)
    (format stream "(defsystem \"~A\"~%" system-name)
    (format stream "  :depends-on ()~%")
    (format stream "  :components ((:file \"~A\")))~%" system-name)))

(defun template-source-file (dir system-name)
  (with-open-file (stream (merge-pathnames (format nil "~A.lisp" system-name) dir)
                          :if-exists :error :direction :output)
    (format stream "(defpackage #:~A~%" system-name)
    (format stream "  (:use :common-lisp)~%")
    (format stream "  (:export))~%")
    (terpri stream)
    (format stream "(in-package #:~A)~%" system-name)))

(defun day-dir (year n)
  (make-pathname :directory (list :relative
                                  (format nil "~D" year)
                                  (format nil "day-~2,'0D" n))))

(defun init-day (n name &optional (year +this-year+))
  (let ((day-dir (day-dir year n)))
    (ensure-directories-exist day-dir)
    (touch (merge-pathnames "input.txt" day-dir))
    (template-defsystem day-dir name)
    (template-source-file day-dir name)))

(defun remove-day (n &optional (year +this-year+))
  (let ((day-dir (day-dir year n)))
    (uiop:delete-directory-tree
     day-dir
     :if-does-not-exist :ignore
     :validate #'(lambda (dir) (yes-or-no-p "Delete directory tree at ~A" dir)))))
