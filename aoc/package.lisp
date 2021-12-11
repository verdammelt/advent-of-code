(defpackage #:aoc
  (:use :cl)
  (:export
   ;; math
   #:sum
   #:product

   ;; strings
   #:empty-string-p
   #:split-lines-on-empty-line
   #:split-string-on-char
   #:split-string-on-chars

   ;; systems
   #:reload-year
   #:load-systems
   #:test-systems

   ;; utils
   #:parsed-day-data
   #:perform-day-task
   #:current-year
   #:current-day
   #:data-pathname
   #:today-data-pathname
   #:read-data
   #:def-today-suite*
   ))
