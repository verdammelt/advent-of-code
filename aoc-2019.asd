;; TODO: "modernize" 2019 exercises
(defsystem "aoc-2019"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("aoc")

  :pathname "2019"
  :serial t
  :components ((:module "utils" :pathname ""
                :components ((:file "aoc")
                             (:file "file-utils")
                             (:file "string-utils")))
               (:module "intcode" :pathname ""
                :serial t
                :components ((:file "computer")
                             (:file "computer-tests")))
               (:module "code" :pathname ""
                :depends-on ("utils" "intcode")
                :components ((:file "day-1/fuel-counter")
                             (:file "day-2/intcode")
                             (:file "day-3/crossed-wires")
                             (:file "day-4/secure-container")
                             (:file "day-5/day5")
                             (:file "day-6/orbit-calc")
                             (:file "day-7/amplification-circuit")
                             (:file "day-8/space-image-format")
                             (:file "day-9/boost")
                             (:file "day-10/monitor-station")
                             (:file "day-11/painting")
                             (:file "day-12/nbody")
                             (:file "day-13/arcade")
                             (:file "day-14/fuel")
                             (:file "day-15/repair-droid")
                             (:file "day-16/ftt")
                             (:file "day-17/flare")
                             (:file "day-19/tractor-beam")
                             ))))
