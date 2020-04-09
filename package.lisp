(defpackage ten/parser
  (:use :cl :esrap)
  (:import-from :split-sequence
                :split-sequence-if)
  (:export :<output-tag>
           :<else-tag>
           :<control-tag>
           :<super-tag>
           :<include-tag>
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
           :%ten-stream))

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
                ;;:super
                ;;:include
                :raw
                :verb
                :verbatim)
  (:export :compile-template
           :template
           :super
           :include
           :raw
           :verb
           :verbatim))

(defpackage #:ten-templates
  (:use :cl :ten))
