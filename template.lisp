;;; ten-template: The package where templates are compiled

(defpackage ten/template
  (:use :cl)
  (:export :template
           :esc))

(in-package :ten/template)

(defun esc (string)
  "Escape a string."
  (who:escape-string string))

(defmacro template (name (&key (escape-html t) inherits-from) args &rest body)
  `(progn
     (defun ,name ,args
       (with-output-to-string (%ten-stream)
         ,(if (not escape-html)
              `(flet ((esc (string)
                        string))
                 ,@body)
              `(progn
                 ,@body))))
     (compile ',name)
     (export ',name (symbol-package ',name))))
