(in-package #:ten)

(defun expand-template (string-or-pathname &optional (package-name ten/compiler:*template-package*))
  "Expand a template to Lisp code. Useful for debugging."
  (ten/compiler:compile-template
   (ten/parser:parse-template string-or-pathname)
   package-name))

(defun compile-template (string-or-pathname &optional (package-name ten/compiler:*template-package*))
  "Compile a template. If a pathname is given, compiles the file content. Otherwise, compiles the given string."
  (let* ((expanded-template (expand-template string-or-pathname package-name)))
    (if (atom (first expanded-template))
        (eval expanded-template)
        (mapcar 'eval expanded-template)))
  t)
