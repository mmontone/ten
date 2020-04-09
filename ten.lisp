(in-package #:ten)

(defun compile-template (string-or-pathname &optional (package-name ten/compiler:*template-package*))
  (let ((compiled-template (ten/compiler:compile-template
                            (ten/parser:parse-template string-or-pathname)
                            package-name)))
    (break "~a" compiled-template)
    (if (atom (first compiled-template))
        (eval compiled-template)
        (mapcar 'eval compiled-template))))
