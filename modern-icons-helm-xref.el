;;; modern-icons-helm-xref.el --- Modern icons for Helm-xref -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Version: 0.1.0
;; Created: July 12, 2025
;; Homepage: https://github.com/taquangtrung/modern-icons.el
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp, icons, vscode-icons, helm, helm-xref

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

;; To enable modern icons for Helm-xref, use the following code:
;;
;;   (require 'modern-icons-helm-xref)
;;   (modern-icons-helm-xref-enable)

;;; Code:

(require 'modern-icons-core)

(defun modern-icons-helm-xref-advisor (func file &rest args)
  "Advice function for `helm-xref' to display icons.
FUNC is one of the 'helm-xref-format-candidate' functions."
  (let ((icon (propertize " " 'display
                          (or (modern-icons-icon-for-file file)
                              (modern-icons-default-file-icon))))
        (res (apply func file args)))
    (concat icon (propertize " " 'display '(space :width 0.5)) res)))

;;;###autoload
(defun modern-icons-helm-xref-enable ()
  "Enable `modern-icons-helm-xref'."
  (interactive)
  (cond
   ((require 'helm-xref nil 'noerror)
    (advice-add 'helm-xref-format-candidate-short :around #'modern-icons-helm-xref-advisor)
    (advice-add 'helm-xref-format-candidate-full-path :around #'modern-icons-helm-xref-advisor)
    (advice-add 'helm-xref-format-candidate-long :around #'modern-icons-helm-xref-advisor)
    (when (called-interactively-p 'any)
      (message "Modern-icons-helm-xref is enabled!")))
   (t (message "Modern-icons-helm-xref: helm-xref isn't installed!"))))

;;;###autoload
(defun modern-icons-helm-xref-disable ()
  "Disable `modern-icons-helm-xref'."
  (interactive)
  (cond
   ((require 'helm-xref nil 'noerror)
    (advice-remove 'helm-xref-format-candidate-short #'modern-icons-helm-xref-advisor)
    (advice-remove 'helm-xref-format-candidate-full-path #'modern-icons-helm-xref-advisor)
    (advice-remove 'helm-xref-format-candidate-long #'modern-icons-helm-xref-advisor)
    (when (called-interactively-p 'any)
      (message "Modern-icons-helm-xref is disabled!")))
   (t (message "Modern-icons-helm-xref: helm-xref isn't installed!"))))

(provide 'modern-icons-helm-xref)
;;; modern-icons-helm-xref.el ends here
