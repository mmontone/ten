(defun ten-compile-template ()
  (interactive)
  (slime-eval `(ten:compile-template (cl::pathname ,(buffer-file-name)))))

(defun ten-expand-template ()
  (interactive)
  (let ((expanded (slime-eval
                   `(cl:with-output-to-string
                     (s)
                     (cl:pprint (ten:expand-template (cl::pathname ,(buffer-file-name))) s)))))
    (slime-with-popup-buffer ("*TEN expansion*"
                              :package :ten-templates :connection t
                              :mode 'lisp-mode)
      (slime-mode 1)
      (slime-macroexpansion-minor-mode 1)
      (setq font-lock-keywords-case-fold-search t)
      (current-buffer)
      (insert expanded))))

(define-minor-mode ten-mode
  "Minor mode for compiling and expanding TEN templates"
  :init-value nil
  :lighter " TEN"
  :keymap
  `((,(kbd "C-c C-c") . ten-compile-template)
    (,(kbd "C-c RET") . ten-expand-template))
  :group 'ten)

;; install like:
;; (add-hook 'web-mode-hook 'ten-mode)
