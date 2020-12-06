(defsystem "aoc-2020"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "verdammelt@gmail.com"

  :depends-on ("split-sequence" "fiveam")

  :pathname "2020"
  :serial t
  :components ((:module "code" :pathname ""
                :components ((:file "utils")
                             (:file "day1")
                             (:file "day2")
                             (:file "day3")
                             (:file "day4")
                             (:file "day5")
                             (:file "day6")))
               (:module "test" :pathname ""
                :components ((:file "tests"))))

  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call '#:aoc-2020/test '#:run-all-tests :on-error :signal)))
