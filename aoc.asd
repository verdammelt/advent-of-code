(defsystem "aoc"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on (
               "alexandria"
               "cl-ppcre"
               "dexador"
               "fiveam"
               "split-sequence"
               "uiop"
               )

  :pathname "aoc"
  :components ((:file "package")
               (:module "utils" :pathname "" :depends-on ("package")
                :components ((:file "arrays")
                             (:file "coords")
                             (:file "math")
                             (:file "sequences")
                             (:file "strings")
                             (:file "symbols")
                             (:file "systems")
                             (:file "utils")))
               (:module "algorithms" :depends-on ("package")
                :components ((:file "dijkstra"))))

  :in-order-to ((test-op (test-op "aoc/test"))))

(load-system "fiveam-asdf")
(defsystem "aoc/test"
  :defsystem-depends-on ("fiveam-asdf")
  :class fiveam-tester-system
  :depends-on ("fiveam" "aoc")

  :test-package :aoc-tests
  :test-names (:aoc-tests)

  :pathname "aoc/tests"
  :components ((:module "base" :pathname ""
                :components ((:file "package") (:file "suite")))
               (:module "utils" :depends-on ("base")
                :components ((:file "arrays")
                             (:file "symbols")))
               (:module "algorithms" :depends-on ("base")
                       :components ((:file "dijkstra")))))
