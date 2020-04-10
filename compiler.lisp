(in-package :ten/compiler)

(defparameter *template-package*
  (find-package 'ten-templates)
  "The package where template expressions are evaluated and the template function is exported")

(defvar *compiling-template*) ;; The template being compiled
(defvar *sections*) ;; Sections of current template

(defgeneric render-section (section-name template stream))

(defun read-template-expressions (string)
  (let ((*package* *template-package*)
        (end '#:eof))
    (with-input-from-string (s string)
      (loop
         for form = (read s nil end)
         until (eq form end)
         collect form))))

(defun extract-filters (string)
  (let ((parts (mapcar 'read-template-expressions (split-sequence:split-sequence #\| string))))
    (values (first parts) (rest parts))))

(defun apply-filters (code filters)
  (loop
     :with result := code
     :for filter in filters
     :do (setf result
               (alexandria:if-let
                   ((pos (position '_ filter :test 'equalp)))
                 (let ((replaced filter))
                   (setf (nth pos replaced) result)
                   replaced)
                 (list* (first filter)
                        result
                        (rest filter))))
     :finally (return result)))
       
;;; Compiler

(defmethod emit ((str string))
  `(write-string ,str %ten-stream))

(defmethod emit ((vec vector))
  `(progn ,@(loop for elem across vec collecting (emit elem))))

(defmethod emit ((tag <output-tag>))
  (multiple-value-bind (expr filters)
      (extract-filters (code tag))
    (alexandria:with-unique-names (out raw)
      `(multiple-value-bind (,out ,raw)
           ,(apply-filters
             (if (= (length expr) 1)
                 (first expr)
                 expr)
             filters)
         (if ,raw
             ;; if the second return value is T, the result of the expression
             ;; is not escaped
             (princ ,out %ten-stream)
             (write-string (esc (princ-to-string ,out))
                           %ten-stream))))))

(defun else-tag-p (element)
  (typep element '<else-tag>))

(defmethod emit ((tag <control-tag>))
  (flet ((emit-body (body)
           (let ((else-tag-pos (position-if 'else-tag-p body)))
             (loop
                for elem in (if else-tag-pos
                                (split-sequence-if 'else-tag-p body)
                                (coerce body 'list))
                collecting (emit elem)))))
    (let ((exprs (read-template-expressions (code tag))))
      (if (eql (first exprs) 'ten/template::section)
          ;; sections are a special case
          (progn
            ;; push the section to the list of sections
            ;; to generate render-section methods later
            (push (list (second exprs)
                        (emit-body (body tag)))
                  *sections*)
            `(ten/compiler::render-section ',(second exprs) ten/template::*rendering-template* %ten-stream))
          ;; else
          `(,@exprs ,@(emit-body (body tag)))))))


(defun control-element-p (element)
  (typep element '<control-tag>))

(defun emit-toplevel (code)
  (emit code))

(defun compile-template (elements &optional (package-name 'ten/template))
  (loop
     for element across elements
     when (not (stringp element))
     appending
       (let ((*template-package* (find-package package-name)))
         (call-with-template-header-options
          element
          (lambda () (emit-toplevel element))))))

(defun start-template-compilation (template-name)
  (declare (ignore template-name)))

(defun finish-template-compilation (template-name result)
  (declare (ignore template-name))
  ;; Handle the sections here
  (append result
          (loop
             for section in *sections*
             collect
               `(defmethod render-section ((section (eql ',(first section)))
                                           (template ,*compiling-template*)
                                           %ten-stream)
                  (declare (ignore section template))
                  ,@(second section)))))

(defun call-with-template-header-options (header func)
  (let* ((expr (read-template-expressions (code header))))
    (if (eql (first expr) 'ten/template:template)
        (destructuring-bind (_ template-name options args)
            expr
          (declare (ignore _ args))
          (let ((*compiling-template* template-name)
                (*template-package* (or (find-package (getf options :package)) *template-package*))
                (*sections* nil))
            (start-template-compilation template-name)
            (let ((compiled-template (funcall func)))
              (finish-template-compilation template-name (list compiled-template)))))
        (funcall func))))
