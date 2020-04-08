(in-package #:ten)

(defun compile-template (string-or-pathname)
  (eval (ten/compiler:compile-template
         (ten/parser:parse-template string-or-pathname))))
