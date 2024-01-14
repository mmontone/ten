(asdf:defsystem #:ten.examples
  :description "Examples for TEN Common Lisp Template System"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :defsystem-depends-on (:ten)
  :depends-on (:ten)
  :components
  ((:module "examples"
            :components
            ((:file "package")
             (:ten-template "ex1" :file-extension "html" :package :ten/examples)
	     (:ten-template "comments" :file-extension "html" :package :ten/examples)
             (:ten-template "inheritance" :file-extension "html" :package :ten/examples)
             (:ten-template "include" :file-extension "html" :package :ten/examples)
             (:ten-template "dot-syntax" :file-extension "html" :package :ten/examples)
             (:ten-template "dot-syntax-2" :file-extension "html" :package :ten/examples)
             (:ten-template "i18n" :file-extension "html" :package :ten/examples)
             (:ten-template "many" :file-extension "html" :package :ten/examples)
             (:ten-template "filters" :file-extension "html" :package :ten/examples)
             (:ten-template "escaping" :file-extension "html" :package :ten/examples)
             (:ten-template "control" :file-extension "html" :package :ten/examples)
             (:ten-template "with-output-whitespace" :file-extension "html" :package :ten/examples)
             ))))
