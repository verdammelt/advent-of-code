(defsystem "aoc-2015"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "mark@defmethod.io"

  :depends-on ("aoc" "split-sequence")

  :pathname "2015"
  :components ((:file "package")
               (:file "apartment" :depends-on ("package"))
               (:file "wrapping" :depends-on ("package"))))
