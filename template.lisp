;;; ten-template: The package where templates are compiled

(in-package :ten/template)

(defvar *escape-html* t)
(defvar *dot-syntax* t)

(defvar *template-output*)
(defvar *rendering-template* nil)
(defvar *compiling-template*)

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
                               extends
                               package
                               control-delimiters
                               output-delimiters)
                            args &rest body)
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
                                          :initarg (intern (symbol-name (second (first k))) :keyword)
                                          :initform (second k)))
                                  keyword)))
           (arg-names (append required
                              (mapcar 'first optional)
                              (when rest
                                (list rest))
                              (mapcar (alexandria:compose 'second 'first)
                                      keyword)))
           (slots-init (loop
                          for arg in arg-names
                          appending (list (intern (symbol-name arg) :keyword) arg)))
           (body (if dot-syntax
                     `((access:with-dot ()
                         ,@body))
                     body)))
      `(progn
         (defclass ,name (,(or extends 'template))
           ,slots)
         ,@(when (not extends)
            `((defmethod render-template ((template ,name) %ten-stream)
               (with-slots ,arg-names template
                  ,@body))))
         (defun ,name ,args
           (let ((*rendering-template* (make-instance ',name ,@slots-init)))
             (with-output-to-string (%ten-stream)
               (render-template *rendering-template*
                                %ten-stream))))
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

(define-symbol-macro super
    (progn
      (call-next-method)
      ""))

(defmacro include (template &rest args)
  `(progn
     (write-string (,template ,@args) %ten-stream)
     ""))
