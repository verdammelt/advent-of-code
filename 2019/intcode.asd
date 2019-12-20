(defsystem "intcode"
  :in-order-to ((test-op (load-op "intcode/test")))
  :depends-on ("utils")
  :components ((:file "computer")))

(defsystem "intcode/test"
  :depends-on ("intcode")
  :components ((:file "computer-tests")))
