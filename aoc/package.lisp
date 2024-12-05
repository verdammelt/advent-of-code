(defpackage #:aoc
  (:use :cl)
  (:export
   ;; arrays
   #:lists->2d-array
   #:map-2d-array
   #:map-2d-array-values
   #:slice-2d-array
   #:print-2d-array
   #:manhattan-distance

   ;; coords
   #:make-coord
   #:coord-x
   #:coord-y
   #:coord-add
   #:coord-in-bounds
   #:coord-aref
   #:coord-safe-aref

   ;; math
   #:sum
   #:product

   ;; sequences
   #:flatten

   ;; strings
   #:empty-string-p
   #:split-lines-on-empty-line
   #:split-string-on-char
   #:split-string-on-chars
   #:number-string->list-of-digits
   #:string-of-numbers->list-of-numbers

   ;; symbols
   #:keywordize
   #:number-or-keyword

   ;; systems
   #:reload-year
   #:load-systems
   #:test-systems
   #:test-system

   ;; utils
   #:today-data-pathname
   #:download-puzzle-input
   #:read-data
   #:def-today-suite*
   #:run-tests

   ;; dijkstra
   #:dijkstra
   ))
