(load-system "fiveam-asdf")
(defsystem "aoc-2015"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :defsystem-depends-on ("fiveam-asdf")
  :class fiveam-tester-system
  :test-package :keyword
  :test-names (:aoc-2015)

  :depends-on ("aoc" "md5")
  :pathname "2015"
  :components ((:module "utils" :pathname ""
                :components ((:file "tests")))
               (:module "problems" :pathname "" :depends-on ("utils")
                :components ((:file "day01") (:file "day02") (:file "day03")
                             (:file "day04") (:file "day05") (:file "day06")
                             (:file "day07") (:file "day08") (:file "day09")
                             (:file "day10")))))
