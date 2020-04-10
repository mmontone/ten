(in-package :ten/template)

(defvar *default-language* :en)
(defvar *current-language* nil)

(defvar *translation-backend* nil "The translation backend.")

(defvar *warn-on-untranslated-messages* t)
(defvar *untranslated-messages* nil)

(defun translate (string &optional args
                           (language (or *current-language* *default-language*))
                           (backend *translation-backend*))
  (apply #'backend-translate backend string language args))

(defun _ (string &rest args)
  (apply #'translate string args))

(defun format-translation (string &rest args)
  (apply #'format nil
         (ppcre:regex-replace-all
          "\\:(\\w*)"
          string
          (lambda (_ varname)
            (declare (ignore _))
            (let ((val (access:access args (make-keyword varname))))
              (or (and val (princ-to-string val))
                  (error "~A missing in ~A translation" varname string))))
          :simple-calls t)
         args))

(defgeneric backend-translate (backend string language &rest args)
  (:method ((backend null) string language &rest args)
    (error "Translation backend has not been setup"))
  (:method ((backend t) string language &rest args)
    (error "Invalid translation backend: ~A" backend)))
