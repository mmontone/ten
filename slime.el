(defun ten-compile-template ()
  (interactive)
  (slime-eval `(ten:compile-template (cl::pathname ,(buffer-file-name)))))
