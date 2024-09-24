;;; modern-icons-helm-rg.el --- Modern icons for Helm-rg -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Version: 0.1.0
;; Created: July 12, 2025
;; Homepage: https://github.com/taquangtrung/modern-icons.el
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp, icons, vscode-icons, helm, helm-rg

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

;; To enable modern icons for Helm-rg, use the following code:
;;
;;   (require 'modern-icons-helm-rg)
;;   (modern-icons-helm-rg-enable)

;;; Code:

(require 'modern-icons)

(defun modern-icons-helm-rg-advisor (func face str &rest args)
  "Advice function for `helm-rg' to display icons. FUNC is ``helm-rg--make-face'."
  (let ((str (cond ((equal face 'helm-rg-file-match-face)
                    (concat (propertize " " 'display (or (modern-icons-icon-for-file str)
                                                         (modern-icons-default-file-icon)))
                            (propertize " " 'display '(space :width 0.5))
                            str))
                   ((equal face 'helm-rg-line-number-match-face)
                    (concat "   " str))
                   (t str))))
    (apply func face str args)))

;;;###autoload
(defun modern-icons-helm-rg-enable ()
  "Enable `modern-icons-helm-rg'."
  (interactive)
  (cond
   ((require 'helm-rg nil 'noerror)
    (advice-add 'helm-rg--make-face :around #'modern-icons-helm-rg-advisor)
    (when (called-interactively-p 'any)
      (message "Modern-icons-helm-rg is enabled!")))
   (t (message "Modern-icons-helm-rg: helm-rg isn't installed!"))))

;;;###autoload
(defun modern-icons-helm-rg-disable ()
  "Disable `modern-icons-helm-rg'."
  (interactive)
  (cond
   ((require 'helm-rg nil 'noerror)
    (advice-remove 'helm-rg--make-face #'modern-icons-helm-rg-advisor)
    (when (called-interactively-p 'any)
      (message "Modern-icons-helm-rg is disabled!")))
   (t (message "Modern-icons-helm-rg: helm-rg isn't installed!"))))

(provide 'modern-icons-helm-rg)
;;; modern-icons-helm-rg.el ends here
