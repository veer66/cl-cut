(in-package :asdf-user)

(asdf:defsystem #:cl-cut
    :description "Macros for partial application of expressions in the spirit of SRFI 26."
    :author "Cromachina"
    :license "MIT"
    :version "1.0.0"
    :pathname "src"
    :components ((:file "cl-cut")))
