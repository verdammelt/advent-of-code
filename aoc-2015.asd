(defsystem "aoc-2015"
  :description ""
  :version "0.0.0"
  :author "Mark Simpson"
  :mailto "mark@defmethod.io"

  :depends-on ("aoc" "split-sequence" "md5")

  :pathname "2015"
  :components ((:module "base"
                        :pathname ""
                        :components ((:file "package")
                                     (:file "apartment" :depends-on ("package"))
                                     (:file "wrapping" :depends-on ("package"))
                                     (:file "delivery" :depends-on ("package"))
                                     (:file "adventcoin" :depends-on ("package"))))
               (:file "solution-tests" :depends-on ("base"))))
