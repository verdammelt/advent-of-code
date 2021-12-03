(defsystem "aoc-2021"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("aoc")

  :pathname "2021"
  :serial t
  :components ((:file "tests")
               (:file "day01"))

  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call '#:aoc-2021/test '#:run-tests)))
