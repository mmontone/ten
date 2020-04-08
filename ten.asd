(asdf:defsystem #:ten
  :description "Template System for Common Lisp"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:access :esrap :cl-who :split-sequence)
  :components ((:file "package")
               (:file "parser")
               (:file "template")
               (:file "compiler")
               (:file "ten")))
