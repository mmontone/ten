(asdf:defsystem #:ten.tests
  :description "Tests for TEN Common Lisp Template System"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:ten :ten.examples :fiveam)
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :ten/tests :run-tests)))
