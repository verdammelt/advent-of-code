(defsystem "aoc-2021"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("aoc" "alexandria")

  :pathname "2021"
  :serial t
  :components ((:file "tests")
               (:file "day01")
               (:file "day02")
               (:file "day03")
               (:file "day04")
               (:file "day05")
               (:file "day06")
               (:file "day07")
               (:file "day08")
               (:file "day09")
               (:file "day10")
               (:file "day11")
               (:file "day12"))

  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call '#:aoc-2021/test '#:run-tests)))
