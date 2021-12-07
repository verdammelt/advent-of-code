(defpackage #:aoc
  (:use :cl)
  (:export
   #:parsed-day-data
   #:perform-day-task
   #:current-year
   #:current-day
   #:data-pathname
   #:today-data-pathname
   #:read-data
   #:reload-year
   #:def-today-suite*

   ;; strings
   #:empty-string-p
   #:split-lines-on-empty-line
   #:split-string-on-char
   #:split-string-on-chars))
