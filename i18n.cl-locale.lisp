(in-package :ten/template)

#-lispworks
(defmethod backend-translate ((backend (eql :cl-locale)) string language &rest args)
  (let ((dictionary (locale:current-dictionary)))
    (when (not (arnesi:aand (not (eq language locale:*default-locale*))
                            (gethash language dictionary)
                            (gethash string arnesi:it)))
      (when *warn-on-untranslated-messages*
        (warn "TRANSLATION NOT GIVEN: ~A ~A" string language))
      (pushnew (cons string language) *untranslated-messages* :test 'equalp)))
  (apply #'format-translation
         (cl-locale:i18n string
                         :locale language
                         :params args)
         args))

(setf *translation-backend* :cl-locale)
