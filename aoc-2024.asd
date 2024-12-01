(load-system "fiveam-asdf")
(defsystem "aoc-2024"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :defsystem-depends-on ("fiveam-asdf")
  :class fiveam-tester-system
  :test-package :keyword
  :test-names (:aoc-2024)

  :depends-on ("aoc")
  :pathname "2024"
  :components ((:module "utils" :pathname ""
                :components ((:file "tests")))
               (:module "problems" :pathname "" :depends-on ("utils")
                :components ((:file "day01")))))
