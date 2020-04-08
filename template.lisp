;;; ten-template: The package where templates are compiled

(defpackage ten/template
  (:use :cl)
  (:export :template
           :esc))

(in-package :ten/template)

(defvar *escape-html* t)
(defvar *dot-syntax* t)

(defun esc (string)
  "Escape a string."
  (who:escape-string string))

(defmacro template (name (&key (escape-html *escape-html*)
                               (dot-syntax *dot-syntax*)
                               inherits-from) args &rest body)
  (let ((output-code
         `(with-output-to-string (%ten-stream)
            ,(if (not escape-html)
                 `(flet ((esc (string)
                           string))
                    ,@body)
                 `(progn
                    ,@body)))))
    `(progn
       (defun ,name ,args
         ,@(if dot-syntax
               `((access:with-dot ()
                   ,output-code))
               output-code))
       (compile ',name)
       (export ',name (symbol-package ',name)))))

(defmacro raw (&body body)
  `(flet ((esc (string)
            string))
     ,@body))
