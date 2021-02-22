(in-package :asdf-user)
(asdf:defsystem #:cl-cut.test
  :description "Tests for cl-cut"
  :author "Cromachina"
  :license "MIT"
  :depends-on (#:cl-cut #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :pathname "test"
  :components ((:test-file "cl-cut"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
