(defsystem "aoc"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("uiop" "cl-ppcre" "split-sequence" "fiveam")

  :pathname "aoc"
  :components ((:file "package")
               (:file "arrays")
               (:file "math")
               (:file "strings")
               (:file "symbols")
               (:file "systems")
               (:file "utils")))
