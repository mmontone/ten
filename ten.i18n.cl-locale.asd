(asdf:defsystem #:ten.i18n.cl-locale
  :description "i18n for TEN Common Lisp Template System using cl-locale"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:ten :cl-locale)
  :components ((:file "i18n.cl-locale")))
