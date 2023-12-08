(load-system "fiveam-asdf")
(defsystem "aoc-2020"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :defsystem-depends-on ("fiveam-asdf")
  :class fiveam-tester-system
  :test-package :keyword
  :test-names (:aoc-2020)

  :depends-on ("aoc" "split-sequence")
  :pathname "2020"
  :components ((:module "utils" :pathname ""
                :components ((:file "tests") (:file "utils")))
               (:module "problems" :pathname "" :depends-on ("utils")
                :components ((:file "day01") (:file "day02") (:file "day03")
                             (:file "day04") (:file "day05") (:file "day06")
                             (:file "day07") (:file "day08") (:file "day09")
                             (:file "day10") (:file "day11") (:file "day12")
                             (:file "day13") (:file "day14") (:file "day15")
                             (:file "day16") (:file "day17") (:file "day18")
                             (:file "day19") (:file "day20") (:file "day22")
                             (:file "day23") (:file "day24")))))
