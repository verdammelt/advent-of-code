(defsystem "aoc-2022"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("aoc")

  :pathname "2022"
  :serial t
  :components ((:file "tests")
               (:file "day01"))

  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call '#:aoc-2022/test '#:run-tests)))
