(defsystem "aoc-2015"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("aoc" "md5" "fiveam")

  :pathname "2015"
  :components ((:file "tests")
               (:file "day01") (:file "day02") (:file "day03") (:file "day04")
               (:file "day05") (:file "day06"))

  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call '#:aoc-2015/test '#:run-tests)))
