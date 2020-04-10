(in-package :ten/template)

(defvar *gettext-domain* nil)

(defmethod backend-translate ((backend (eql :gettext)) string language &rest args)
  (apply #'format-translation
         (gettext:gettext* string *gettext-domain* nil (string-downcase (string language)))
         args))

(setf *translation-backend* :gettext)
