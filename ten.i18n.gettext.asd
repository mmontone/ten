(asdf:defsystem #:ten.i18n.gettext
  :description "i18n for TEN Common Lisp Template System using gettext"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:ten :gettext)
  :components ((:file "i18n.gettext")))
