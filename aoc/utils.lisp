(in-package #:aoc)

(defun current-year ()
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "AOC-(\\d+)" (package-name *package*))
    (if match
        (elt groups 0)
        (error "Current package not in form AOC-YYYY"))))

(defun current-day ()
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "AOC-(\\d+)-(\\d+)" (package-name *package*))
    (if match
        (elt groups 1)
        (error "Current package not in form AOC-YYYY-DD"))))

(defun data-file-pathname (year day &optional modifier (type "txt"))
  (make-pathname
   :directory (list :relative (format nil "~4,'0D" year) "inputs")
   :name (format nil "day~2,'0D~@[-~A~]" day modifier)
   :type type))

(defun today-data-pathname (&optional modifier (type "txt"))
  (data-file-pathname (current-year) (current-day) modifier type))

(defun read-data (pathname &key (line-parser #'identity)
                             (pre-process #'identity)
                             (post-process #'identity))
  "Read data lines from PATHNAME. Apply PRE-PROCESS to all lines, then
LINE-PARSER to each line and finally POST-PROCESS to all lines before returning."
  (funcall post-process
           (mapcar line-parser
                   (funcall pre-process (uiop:read-file-lines pathname)))))

(defvar *session-cookie*)
(defvar *user-agent*)

(defun download-puzzle-input (year day &key (if-exists :error))
  (let ((url (format nil "https://adventofcode.com/~D/day/~D/input" year day))
        (pathname (data-file-pathname year day)))
    (ensure-directories-exist pathname :verbose t)
    (uiop:with-output-file (output pathname :if-exists if-exists :if-does-not-exist :create)
      (write-string
       (dex:get url :headers `(("User-Agent" . aoc::*user-agent*)
                               ("Cookie" . ,(format nil "session=~A" aoc::*session-cookie*))))
       output))
    pathname))

(defun year-day-keyword (year &optional day)
  (keywordize (format nil "AOC-~4,'0D~@[-~2,'0D~]" year day)))

(defmacro def-today-suite* ()
  `(5am:def-suite* ,(year-day-keyword (current-year) (current-day))
     :in ,(year-day-keyword (current-year))))

(defun run-tests (year &optional day)
  (5am:run! (year-day-keyword year day)))
