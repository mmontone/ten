(asdf:defsystem #:ten.examples
  :description "Examples for TEN Common Lisp Template System"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:ten)
  :components ((:module "examples"
                        :components
                        ((:file "package")
                         (:ten-template "ex1"
                                        :file-extension "html"
                                        :package :ten/examples)))))
