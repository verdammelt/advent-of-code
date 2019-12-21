(defsystem "tractor-beam"
  :depends-on ("intcode")
  :in-order-to ((test-op (load-op "tractor-beam/test")))
  :components ((:file "tractor-beam")))
