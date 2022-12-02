(in-package #:aoc)

(defun all-systems ()
  (mapcar #'pathname-name
          (remove-if #'(lambda (p) (string= (pathname-name p) "aoc"))
                     (directory "*.asd"))))

(defun load-systems (&key force)
  (dolist (sys (all-systems))
    (if force (asdf:load-system sys)
        (ql:quickload sys))))

(defun test-systems (&key force)
  (dolist (sys (all-systems))
    (asdf:test-system sys :force force)))

(defun reload-year (year)
  "Reload the system for YEAR. Removes any packages matching AOC-<YEAR>"
  (let* ((prefix (format nil "aoc-~4,'0D" year))
         (scanner (cl-ppcre:create-scanner prefix :case-insensitive-mode t)))
    (let ((year-packages (remove-if-not #'(lambda (p) (cl-ppcre:scan scanner (package-name p)))
                                        (list-all-packages))))
      (mapc #'(lambda (p) (mapc #'(lambda (from) (unuse-package p from)) year-packages))
            year-packages)
      (mapc #'delete-package year-packages))
    (asdf:make prefix :force t)))
