(defsystem "aoc-2022"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("aoc")

  :pathname "2022"
  :components ((:module "utils" :pathname ""
                :components ((:file "tests")))
               (:module "problems" :pathname "" :depends-on ("utils")
                :components ((:file "day01") (:file "day02") (:file "day03"))))

  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call '#:aoc-2022/test '#:run-tests)))
