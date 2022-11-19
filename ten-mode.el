;;; ten-mode.el --- Minor mode for compiling TEN Lisp templates  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode for compiling TEN Lisp templates
;;
;; install like:
;; (add-hook 'web-mode-hook 'ten-mode)

;;; Code:

(require 'slime)

(defun ten-compile-template ()
  "Compile the TEN template in `current-buffer'."
  (interactive)
  (slime-eval `(ten:compile-template (cl::pathname ,(buffer-file-name))))
  (message "TEN template compiled."))

(defun ten-expand-template ()
  "Expand the TEN template in `current-buffer'."
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
  "Minor mode for compiling and expanding TEN templates."
  :init-value nil
  :lighter " TEN"
  :keymap
  `((,(kbd "C-c C-c") . ten-compile-template)
    (,(kbd "C-c RET") . ten-expand-template))
  :group 'ten)

(provide 'ten-mode)
;;; ten-mode.el ends here
