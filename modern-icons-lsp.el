;;; modern-icons-lsp.el --- Modern icons for lsp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Version: 0.1.0
;; Created: June 14, 2025
;; Homepage: https://github.com/taquangtrung/modern-icons.el
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp, icons, lsp

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
;;   (require 'modern-icons-lsp)
;;   (modern-icons-lsp-enable)

;;; Code:

(require 'lsp-icons)
(require 'modern-icons)

(defun modern-icons-lsp-kind-name (kind-index)
  "Get lsp-item-kind name of an LSP KIND-INDEX."
  (cl-case kind-index
    (1 "document")
    (2  "namespace")
    (3  "namespace")
    (4  "namespace")
    (5  "class")
    (6  "method")
    (7  "property")
    (8  "field")
    (9  "method")
    (10 "enum")
    (11 "interface")
    (12 "method" )
    (13 "variable")
    (14 "constant")
    (15 "string")
    (16 "numeric")
    (17 "boolean")
    (18 "boolean")
    (19 "namespace")
    (20 "indexer")
    (21 "boolean")
    (22 "enum member")
    (23 "struct")
    (24 "event")
    (25 "operator")
    (26 "template")
    (t "misc")))

(defun modern-icons-lsp-file-icon-advisor (func file-ext &optional feature &rest args)
  "Advice function for LSP to display file icons.
FUNC is a function of `lsp-icons'that returns file icons, such as the
`lsp-icons-get-by-file-ext' function."
  (when-let* ((_ file-ext)
              (_ (lsp-icons--enabled-for-feature feature))
              (icon (modern-icons-icon-for-file (concat "1." file-ext))))
    (propertize " " 'display icon)))

(defun modern-icons-lsp-symbol-icon-advisor (func kind &optional feature &rest args)
  "Advice function for LSP to display symbol icons.
FUNC is a function of `lsp-icons'that returns file icons, such as the `lsp-icons-get-by-symbol-kind' function."
  (when-let* ((_ kind)
              (_ (lsp-icons--enabled-for-feature feature))
              (item-kind (modern-icons-lsp-kind-name kind))
              (icon (modern-icons-icon-for-code-item item-kind)))
    (propertize " " 'display icon)))

;;;###autoload
(defun modern-icons-lsp-enable ()
  "Enable `modern-icons-lsp'."
  (interactive)
  (advice-add 'lsp-icons-get-by-file-ext :around #'modern-icons-lsp-file-icon-advisor)
  (advice-add 'lsp-icons-get-by-symbol-kind :around #'modern-icons-lsp-symbol-icon-advisor)
  (when (called-interactively-p 'any)
    (message "Modern-icons-lsp is enabled!")))

;;;###autoload
(defun modern-icons-lsp-disable ()
  "Disable `modern-icons-lsp'."
  (interactive)
  (advice-remove 'lsp-icons-get-by-file-ext #'modern-icons-lsp-file-icon-advisor)
  (advice-remove 'lsp-icons-get-by-symbol-kind #'modern-icons-lsp-symbol-icon-advisor)
  (when (called-interactively-p 'any)
    (message "Modern-icons-lsp is disabled!")))

(provide 'modern-icons-lsp)
;;; modern-icons-lsp.el ends here
