(defsystem "aoc-2020"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("aoc" "split-sequence" "fiveam")

  :pathname "2020"
  :serial t
  :components ((:module "code" :pathname ""
                :components ((:file "utils")
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
                             (:file "day12")
                             (:file "day13")
                             (:file "day14")))
               (:module "test" :pathname ""
                :components ((:file "tests"))))

  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call '#:aoc-2020/test '#:run-all-tests :on-error :signal)))
