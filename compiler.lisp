(defpackage ten/compiler
  (:use :cl :ten/parser :ten/template)
  (:import-from :split-sequence
                :split-sequence-if)
  (:import-from :ten/template
                :%ten-stream)
  (:export :compile-template))

(in-package :ten/compiler)

(defparameter *template-package*
  (find-package 'ten/template))

(defun read-template-expressions (string)
  (let ((*package* *template-package*)
        (end '#:eof))
    (with-input-from-string (s string)
      (loop
         for form = (read s nil end)
         until (eq form end)
         collect form))))

;;; Compiler

(defmethod emit ((str string))
  `(write-string ,str %ten-stream))

(defmethod emit ((vec vector))
  `(progn ,@(loop for elem across vec collecting (emit elem))))

(defmethod emit ((tag <output-tag>))
  (let ((expressions (read-template-expressions (code tag))))
    (alexandria:with-unique-names (string-stream)
      `(write-string
        (esc
         (with-output-to-string (,string-stream)
           (princ ,(if (= (length expressions) 1)
                       (first expressions)
                       expressions)
                  ,string-stream)))
        %ten-stream))))

(defun else-tag-p (element)
  (typep element '<else-tag>))

(defmethod emit ((tag <control-tag>))
  (let ((body (body tag)))
    (let ((else-tag-pos (position-if 'else-tag-p body)))
      `(,@(read-template-expressions (code tag))
        ,@(loop
             for elem in (if else-tag-pos
                             (split-sequence-if 'else-tag-p body)
                             (coerce body 'list))
             collecting (emit elem))))))

(defun control-element-p (element)
  (typep element '<control-tag>))

(defun emit-toplevel (code)
  (emit (aref code 0)))

;;; Interface

(defun compile-template (element &optional (package-name 'ten/template))
  (let ((*template-package* (find-package package-name)))
    (emit-toplevel element)))
