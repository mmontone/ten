(in-package #:ten)

(defun compile-template (string-or-pathname &optional (package-name ten/compiler:*template-package*))
  (eval (ten/compiler:compile-template
         (ten/parser:parse-template string-or-pathname)
         package-name)))
