;;; modern-icons-helm-ag.el --- Modern icons for Helm-ag -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Version: 0.1.0
;; Created: July 12, 2025
;; Homepage: https://github.com/taquangtrung/modern-icons.el
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp, icons, vscode-icons, helm, helm-ag

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

;; To enable modern icons for Helm-ag, use the following code:
;;
;;   (require 'modern-icons-helm-ag)
;;   (modern-icons-helm-ag-enable)

;;; Code:

(require 'modern-icons)

(defun modern-icons-helm-ag-advisor (func &rest args)
  "Advice function for `helm-ag' to display icons. FUNC is `helm-ag--filter-one'."
  (let* ((res (apply func args))
         (display (if (consp res) (car res) res))
         (candidate (if (consp res) (cdr res) res)))
    (if-let* ((components (split-string display ":"))
              (_ (length> components 2))
              (file (cl-first components))
              (icon (propertize " " 'display (or (modern-icons-icon-for-file file)
                                                 (modern-icons-default-file-icon))))
              (display (concat icon
                               (propertize " " 'display '(space :width 0.5))
                               display)))
        (cons display candidate)
      res)))


;;;###autoload
(defun modern-icons-helm-ag-enable ()
  "Enable `modern-icons-helm-ag'."
  (interactive)
  (cond
   ((require 'helm-ag nil 'noerror)
    (advice-add 'helm-ag--filter-one :around #'modern-icons-helm-ag-advisor)
    (when (called-interactively-p 'any)
      (message "Modern-icons-helm-ag is enabled!")))
   (t (message "Modern-icons-helm-ag: helm-ag isn't installed!"))))

;;;###autoload
(defun modern-icons-helm-ag-disable ()
  "Disable `modern-icons-helm-ag'."
  (interactive)
  (cond
   ((require 'helm-ag nil 'noerror)
    (advice-remove 'helm-ag--filter-one #'modern-icons-helm-ag-advisor)
    (when (called-interactively-p 'any)
      (message "Modern-icons-helm-ag is disabled!")))
   (t (message "Modern-icons-helm-ag: helm-ag isn't installed!"))))

(provide 'modern-icons-helm-ag)
;;; modern-icons-helm-ag.el ends here
