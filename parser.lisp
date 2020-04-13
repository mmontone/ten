(in-package :ten/parser)

;;; Utilities

(defparameter +whitespace+
  (list #\Space #\Tab #\Newline #\Linefeed #\Backspace
        #\Page #\Return #\Rubout))

(defun whitespacep (char)
  (member char +whitespace+))

(defun trim-whitespace (str)
  (string-trim +whitespace+ str))

;;; Element classes

(defclass <tag> () ())

(defclass <output-tag> (<tag>)
  ((code :reader code :initarg :code)))

(defclass <fcall-tag> (<output-tag>)
  ())

(defclass <var-tag> (<output-tag>)
  ())

(defclass <control-tag> (<tag>)
  ((code :reader code :initarg :code)
   (body :reader body :initarg :body :initform nil)))

(defclass <end-tag> (<tag>)
  ())

(defclass <else-tag> (<tag>) ())

(defmethod print-object ((tag <output-tag>) stream)
  (format stream "<~a ~a>"
          (class-name (class-of tag))
          (code tag)))

(defmethod print-object ((tag <control-tag>) stream)
  (format stream "<~a ~a ~a>"
          (class-name (class-of tag))
          (code tag)
          (body tag)))

;;; Parsing rules

(defparameter +start-output-delimiter+ "{{")
(defparameter +end-output-delimiter+ "}}")
(defparameter +start-control-delimiter+ "{%")
(defparameter +end-control-delimiter+ "%}")

(defrule control-string (+ (not "%}"))
  (:text t))

(defrule control-tag (and "{%";;+start-control-delimiter+
                          control-string
                          "%}";;+end-control-delimiter+
                          )
  (:destructure (open code close)
                (declare (ignore open close))
                (let ((text (trim-whitespace code)))
                  (cond
                    ((equal text "end")
                     (make-instance '<end-tag>))
                    ((equal text "else")
                     (make-instance '<else-tag>))                    
                    (t (make-instance '<control-tag> :code text))))))

(defrule output-string (+ (not "}}"))
  (:lambda (list) (text list)))

(defrule output-tag (and "{{";;+start-output-delimiter+
                          output-string
                          "}}";;+end-output-delimiter+
                          )
  (:destructure (open code close)
                (declare (ignore open close))
                (let ((text (trim-whitespace code)))
                  (if (find #\space text)
                      (make-instance '<fcall-tag> :code text)
                      (make-instance '<var-tag> :code text)))))

(defrule raw-text (+ (not (or "{{" ;;+start-output-delimiter+)
                           "{%") ;;+start-control-delimiter+)
                          ))
  (:lambda (list) (text list)))

(defrule expr (+ (or control-tag output-tag raw-text)))

(defun tokenize-template (string)
  (parse 'expr string))

(defun def-control-without-body (symbol)
  (setf (getf (symbol-plist symbol)
              :ten-control-without-body)
        t))

;;; Token parsing
;;; Take a list of either strings or <tag>s and turn it into a tree

(defun parse-tokens (tokens)
  (let ((tokens (copy-list tokens)))
    (labels ((next-token ()
               (prog1 (first tokens)
                 (setf tokens (rest tokens))))
             (rec-parse (&optional toplevel)
               (let ((out (make-array 1 :adjustable 1 :fill-pointer 0))
                     (tok (next-token)))
                 (loop while (and tok (not (typep tok '<end-tag>))) do
                   (vector-push-extend
                    (cond
                      ((typep tok '<control-tag>)
                       ;; Start a block
                       (make-instance (class-of tok)
                                      :code (code tok)
                                      :body (rec-parse)))
                      (t tok))
                    out)
                   (setf tok (next-token))
                   (when (and (not tok) (not toplevel)) ;; Next tok is nil
                     (error "Missing 'end' tag" )))
                 out)))
      (rec-parse t))))

(defun parse-template (string-or-pathname)
  (if (pathnamep string-or-pathname)
      (parse-template (alexandria:read-file-into-string string-or-pathname))
      (parse-tokens (tokenize-template string-or-pathname))))
