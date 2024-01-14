(in-package :ten/template)

(defvar *escape-html* t)
(defvar *dot-syntax* t)
(defvar *output-whitespace* t)
(defvar *export-template* t "Export the templates by default")

(defvar *template-output* "The stream stream writing template functions write to")
(defvar *compiling-template*)
(defvar *create-string-writing-functions* t)
(defvar *create-stream-writing-functions* nil)

(defclass template ()
  ())

(defun string-writing-function-name (name)
  name)

(defun stream-writing-function-name (name)
  (intern (format nil "~a*" name)
          (symbol-package name)))

(defgeneric render-template (template stream))

(defun esc (string)
  "Escape a string."
  (if *escape-html*
      (who:escape-string string)
      string))

(defun lambda-list-slots (args)
  (multiple-value-bind (required optional rest keyword)
      (alexandria:parse-ordinary-lambda-list args)
    (let* ((slots (append (mapcar (lambda (r)
                                    (list r :initarg (intern (symbol-name r) :keyword))) required)
                          (mapcar (lambda (o)
                                    (list (first o)
                                          :initarg (intern (symbol-name (first o)) :keyword)
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
                         appending (list (intern (symbol-name arg) :keyword) arg))))
      (values slots slots-init arg-names))))

(defmacro template (name (&key extends
                            package
                            (escape-html *escape-html*)
                            (export *export-template*)
                            (dot-syntax *dot-syntax*)
                            (output-whitespace *output-whitespace*)
                            (create-string-writing-function *create-string-writing-functions*)
                            (create-stream-writing-function *create-stream-writing-functions*))
                    args &rest body)

  "The main template creation macro.

Arguments:

- EXTENDS: the name of the template to extend from.
- PACKAGE: the package within which the compiled template is to be defined.
- EXPORT: when T, the generated template function is exported.
- ESCAPE-HTML: whether to escape HTML or not. Default is controlled by *ESCAPE-HTML*, which is true by default.
- OUTPUT-WHITESPACE: whether to output whitespaces or not. Default is controlled by *OUTPUT-WHITESPACE*, which is true by default.
- CREATE-STRING-WRITING-FUNCTION: controls whether a function that writes the template to a string should be created. Default is T.
- CREATE-STREAM-WRITING-FUNCTION: controls whether a function that writes the template to *TEMPLATE-OUTPUT* stream should be created. Default is false.

IMPORTANT: some of this macro arguments are processed by CALL-WITH-TEMPLATE-HEADER-OPTIONS, not here. That's why we declare some of them as ignored."

  (declare (ignore package output-whitespace escape-html))

  (multiple-value-bind (slots slots-init arg-names)
      (lambda-list-slots args)
    (let ((body (if dot-syntax
                    `((access:with-dot ()
                        ,@body))
                    body)))
      `(progn

         (defclass ,name (,(or extends 'template))
           ,slots)

         ,@(when (not extends)
             `((defmethod render-template ((%ten-template ,name) %ten-stream)
                 (with-slots ,arg-names %ten-template
                   ,@body))))

         ,@(when create-string-writing-function
             (let ((fname (string-writing-function-name name)))
               `((defun ,fname ,args
                   (let ((%ten-template (make-instance ',name ,@slots-init)))
                     (values
                      (with-output-to-string (%ten-stream)
                        (render-template %ten-template %ten-stream))
                      t)))
                 (compile ',fname)
                 ,@(when export
                     `((export ',fname (symbol-package ',name)))))))

         ,@(when create-stream-writing-function
             (let ((fname (stream-writing-function-name name)))
               `((defun ,fname ,args
                   (let ((%ten-template (make-instance ',name ,@slots-init)))
                     (values
                      (render-template %ten-template *template-output*))
                     t))
                 (compile ',fname)
                 ,@(when export
                     `((export ',fname (symbol-package ',name)))))))
         ))))

(defmacro begin-raw (&body body)
  `(let ((*escape-html* nil))
     ,@body))

(defmacro begin-verb (&body body)
  `(flet ((esc (string)
            string))
     ,@body))

(defmacro begin-verbatim (&body body)
  `(flet ((esc (string)
            string))
     ,@body))

(defmacro comment (&body body)
  (declare (ignore body))
  "")

(defun raw (str)
  (values str t))

(defun verb (str)
  (values str t))

(defun verbatim (str)
  (values str t))

(define-symbol-macro super
    (progn
      (call-next-method)
      ""))

(defmacro with-output-whitespace (value &body body)
  `(let ((*output-whitespace* ,value))
     ,@body))
