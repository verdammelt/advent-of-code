(defsystem "aoc"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("uiop")

  :pathname "aoc"
  :serial t
  :components ((:file "package")
               (:file "utils")))
