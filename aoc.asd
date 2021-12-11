(defsystem "aoc"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("uiop" "cl-ppcre" "split-sequence" "fiveam")

  :pathname "aoc"
  :serial t
  :components ((:file "package")
               (:file "math")
               (:file "strings")
               (:file "systems")
               (:file "utils")))
