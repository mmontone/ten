;;; ten-template: The package where templates are compiled

(defpackage ten/template
  (:use :cl)
  (:export :template
           :esc))

(in-package :ten/template)

(defvar *escape-html* t)
(defvar *dot-syntax* t)

(defvar *template-output*)

(defclass template ()
  ())

(defgeneric render-template (template stream))

(defun esc (string)
  "Escape a string."
  (if *escape-html*
      (who:escape-string string)
      string))

(defmacro template (name (&key (escape-html *escape-html*)
                               (dot-syntax *dot-syntax*)
                               extends) args &rest body)
  (multiple-value-bind (required optional rest keyword)
      (alexandria:parse-ordinary-lambda-list args)
    (let* ((slots (append (mapcar (lambda (r)
                                    (list r :initarg (intern (symbol-name r) :keyword))) required)
                          (mapcar (lambda (o)
                                    (list (first o)
                                          :initarg (intern (symbol-name o) :keyword)
                                          :initform (second o)))
                                  optional)
                          (when rest
                            (list rest :initarg (intern (symbol-name rest) :keyword)))
                          (mapcar (lambda (k)
                                    (list (second (first k))
                                          :initarg (intern (symbol-name k) :keyword)
                                          :initform (second k)))
                                  keyword)))
           (arg-names (append required
                              (mapcar 'first optional)
                              (when rest
                                (list rest))
                              (mapcar (alexandria:compose 'first 'second)
                                      keyword)))
           (slots-init (loop
                          for arg in arg-names
                          appending (list (intern (symbol-name arg) :keyword) arg))))
      `(progn
         (defclass ,name (,(or extends 'template))
           ,slots)
         (defmethod render-template ((template ,name) %ten-stream)
           (with-slots ,arg-names template
             (access:with-dot ()
               ,@body)))
         (defun ,name ,args
           (with-output-to-string (%ten-stream)
             (render-template (make-instance ',name ,@slots-init)
                              %ten-stream)))
         (compile ',name)
         (export ',name (symbol-package ',name))))))

(defmacro raw (&body body)
  `(let ((*escape-html* nil))
     ,@body))

(defmacro verb (&body body)
  `(flet ((esc (string)
            string))
     ,@body))

(defmacro verbatim (&body body)
  `(flet ((esc (string)
            string))
     ,@body))

(defmacro include (template-name &rest args)
  `(progn
     (render-template
      (make-instance ',template-name ,@args)
      %ten-stream)
     ""))
