;;; modern-icons-corfu.el --- Modern icons for corfu completion -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Version: 0.1.0
;; Created: April 17, 2025
;; Homepage: https://github.com/taquangtrung/modern-icons.el
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp, icons, corfu

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this package, simply install and add this to your init.el
;;
;; (require 'modern-icons-corfu)
;; (add-to-list 'corfu-margin-formatters #'modern-icons-corfu-formatter)

;; Acknowledgement:
;;
;; This package is inspired by the following packages:
;; - nerd-icons-corfu: <https://github.com/LuigiPiucco/nerd-icons-corfu>

;;; Code:

(require 'corfu)
(require 'modern-icons-core)

;; Redefine corfu variables to silent warnings
(defvar corfu-margin-formatters)

;;;###autoload
(defun modern-icons-corfu-formatter (_)
  "A margin formatter for Corfu, adding an icon to the completion item."
  (and-let* ((kindfunc (plist-get completion-extra-properties :company-kind)))
    (lambda (cand)
      (let* ((kind (format "%s" (funcall kindfunc cand)))
             (icon (modern-icons-icon-for-code-item kind))
             (icon-str (propertize " " 'display icon))
             (sep (if (display-graphic-p)
                      ;; Use a narrow whitespace for graphical display
                      (propertize " " 'display '(space :width 0.5))
                    " ")))
        (concat sep icon-str sep)))))

(defun modern-icons-corfu-enable ()
  "Enable `modern-icons-corfu'."
  (interactive)
  (add-to-list 'corfu-margin-formatters #'modern-icons-corfu-formatter)
  (when (called-interactively-p 'any)
    (message "Modern-icons-corfu is enabled!")))

(defun modern-icons-corfu-disable ()
  "Disable `modern-icons-corfu'."
  (interactive)
  (setq corfu-margin-formatters
        (delete #'modern-icons-corfu-formatter corfu-margin-formatters))
  (when (called-interactively-p 'any)
    (message "Modern-icons-corfu is disabled!")))

(provide 'modern-icons-corfu)

;;; modern-icons-corfu.el ends here
