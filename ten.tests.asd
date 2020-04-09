(asdf:defsystem #:ten.tests
  :description "Tests for TEN Common Lisp Template System"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:ten :ten.examples)
  :components ((:file "tests")))
