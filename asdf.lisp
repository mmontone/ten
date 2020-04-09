;;;; This is largely structured around the CFFI Groveller:
;;;;
;;;;     https://github.com/cffi/cffi/blob/master/grovel/asdf.lisp
;;;;

(in-package :asdf)

(defclass ten-template (source-file)
  ((type :initform "ten"
         :initarg :file-extension)
   (package :initform :ten-template
            :initarg :package
            :reader template-package)))

(defmethod compiled-template-path ((component ten-template))
  (make-pathname :type "lisp"
                 :defaults (component-pathname component)))

(defmethod output-files (op (component ten-template))
  nil)

(defmethod perform ((op compile-op) (component ten-template))
  (let ((compiled-template-path (compiled-template-path component)))
    (with-open-file (stream compiled-template-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let* ((parsed (ten/parser:parse-template
                      (component-pathname component)))
             (compiled (ten/compiler:compile-template
                        parsed
                        (template-package component)))
             ;; Need to maintain reference EQuality for uninterned symbols.
             (*print-circle* t))
        (if (atom (first compiled))
            (print compiled stream)
            (mapcar (lambda (code)
                      (print code stream))
                    compiled))))))

(defmethod perform ((op load-op) (component ten-template))
  (let ((compiled-template-path (compiled-template-path component)))
    (perform 'load-source-op
             (make-instance 'cl-source-file
                            :name (component-name component)
                            :parent (component-parent component)
                            :pathname compiled-template-path))))

(import 'ten-test :asdf)
