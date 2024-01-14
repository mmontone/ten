(defpackage ten/parser
  (:use :cl :esrap)
  (:import-from :split-sequence
                :split-sequence-if)
  (:export :<output-tag>
           :<else-tag>
           :<control-tag>
           :code
           :body
           :parse-template))

(defpackage ten/template
  (:use :cl)
  (:export :template
           :esc
           :raw
           :verb
           :verbatim
           :begin-raw
           :begin-verbatim
           :begin-verb
           :super
           :section
           :comment
           :with-output-whitespace
           :_
           :%ten-stream
           :*template-output*
           :*escape-html*
           :*dot-syntax*
           :*create-string-writing-functions*
           :*create-stream-writing-functions*))

(defpackage ten/compiler
  (:use :cl :ten/parser :ten/template)
  (:import-from :split-sequence
                :split-sequence-if)
  (:import-from :ten/template
                :%ten-stream)
  (:export :compile-template
           :*template-package*))

(defpackage #:ten
  (:use #:cl)
  (:import-from :ten/template
                :template
                :esc
                :raw
                :verb
                :verbatim
                :comment
                :super
                :with-output-whitespace
                :section
                :begin-raw
                :begin-verb
                :begin-verbatim
                :_)
  (:export :expand-template
           :compile-template
           :template
           :esc
           :raw
           :verb
           :verbatim
           :comment
           :super
           :section
           :with-output-whitespace
           :begin-raw
           :begin-verb
           :begin-verbatim
           :_))

(defpackage #:ten-templates
  (:use :cl :ten/template)
  (:export :super))
