(defun ten-compile-template ()
  (interactive)
  (slime-eval `(ten:compile-template (cl::pathname ,(buffer-file-name)))))

;; TODO: improve this function
(defun ten-expand-template ()
  (interactive)
  (let ((expanded (slime-eval `(ten:expand-template (cl::pathname ,(buffer-file-name))))))
    (slime-with-popup-buffer ("*TEN expansion*"
                              :package :ten-templates :connection t
                              :mode 'lisp-mode)
      (slime-mode 1)
      (slime-macroexpansion-minor-mode 1)
      (setq font-lock-keywords-case-fold-search t)
      (current-buffer)
      (print expanded))))
