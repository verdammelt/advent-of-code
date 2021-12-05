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

(defun data-pathname (name type)
  "Returns the pathname of the data file with name NAME and type TYPE for the current year."
  (make-pathname :directory (list :relative (current-year) "inputs")
                 :name name
                 :type type))

(defun today-data-pathname (&optional modifier (type "txt"))
  (aoc:data-pathname
   (format nil "day~2,'0D~@[-~A~]" (aoc:current-day) modifier)
   type))


(defun read-data (pathname &key (line-parser #'identity)
                             (pre-process #'identity)
                             (post-process #'identity))
  "Read data lines from PATHNAME. Apply PRE-PROCESS to all lines, then
LINE-PARSER to each line and finally POST-PROCESS to all lines before returning."
  (funcall post-process
           (mapcar line-parser
                   (funcall pre-process (uiop:read-file-lines pathname)))))

(defun reload-year (year)
  (let* ((prefix (format nil "aoc-~4,'0D" year))
         (scanner (cl-ppcre:create-scanner prefix :case-insensitive-mode t)))
    (let ((year-packages (remove-if-not #'(lambda (p) (cl-ppcre:scan scanner (package-name p)))
                                        (list-all-packages))))
      (mapc #'(lambda (p) (mapc #'(lambda (from) (unuse-package p from)) year-packages))
            year-packages)
      (mapc #'delete-package year-packages))
    (asdf:make prefix :force t)))

(defun current-year-day-keyword ()
  (alexandria:make-keyword (format nil "AOC-~4,'0D-~2,'0D" (current-year) (current-day))))

(defun current-year-keyword ()
  (alexandria:make-keyword (format nil "AOC-~4,'0D" (current-year))))

(defmacro def-today-suite* ()
  `(5am:def-suite* ,(current-year-day-keyword) :in ,(current-year-keyword)))
