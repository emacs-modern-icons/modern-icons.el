;;; modern-icons.el --- Emacs modern and pretty icons library -*- lexical-binding: t -*-

;; Copyright (C) 2025, Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Version: 0.1.0
;; Created: September 25, 2024
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/taquangtrung/modern-icons.el
;; Keywords: lisp, icons

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

;; This library provides modern and pretty icons in SVG format for displaying in Emacs.

;;; Code:

(require 'cl-lib)
(require 'tar-mode)
(require 'modern-icons-core)

(defgroup modern-icons nil
  "Modern icons for Emacs."
  :prefix "modern-icons-"
  :group 'appearance
  :group 'convenience)

(defcustom modern-icons-enable-packages
  '(corfu dired helm helm-xref lsp treemacs)
  "List of Emacs packages to enable displaying modern icons. The current
 supported packages are `corfu', `dired', `helm',`helm-ag', `helm-rg',
`helm-xref', `lsp', `treemacs'"
  :group 'modern-icons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up

(defun modern-icons-extract-icons ()
  "Extract all icons from the tar file into the `icons' directory."
  (interactive)
  (let ((icons-tar-file (concat modern-icons-root-dir modern-icons-tar-file))
        (message-log-max nil)
        (inhibit-message t))
    (with-current-buffer (find-file-noselect icons-tar-file)
      (tar-untar-buffer)
      (kill-buffer))))

(defun modern-icons-package-icons ()
  "Package all icons from the `icons' directory into the tar file."
  (interactive)
  (message "modern-icons: packaging icons files...")
  (if (executable-find "tar")
      (let ((default-directory modern-icons-root-dir))
        (shell-command (format "tar -cJf %s %s" modern-icons-tar-file
                               modern-icons-icons-dir)))
    (message "Error: require `tar` command!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions

(defun modern-icons-enable ()
  "Enable all `modern-icons' features."
  (interactive)

  ;; Extract icons using a timer to ensure the tar file is opened properly.
  (run-at-time 0.1 nil (lambda () (modern-icons-extract-icons)))

  (when (member 'corfu modern-icons-enable-packages)
    (require 'modern-icons-corfu)
    (modern-icons-corfu-enable))

  (when (member 'dired modern-icons-enable-packages)
    (require 'modern-icons-dired)
    (modern-icons-dired-enable))

  (when (member 'helm modern-icons-enable-packages)
    (require 'modern-icons-helm)
    (modern-icons-helm-enable))

  (when (member 'helm-ag modern-icons-enable-packages)
    (require 'modern-icons-helm-ag)
    (modern-icons-helm-ag-enable))

  (when (member 'helm-rg modern-icons-enable-packages)
    (require 'modern-icons-helm-rg)
    (modern-icons-helm-rg-enable))

  (when (member 'helm-xref modern-icons-enable-packages)
    (require 'modern-icons-helm-xref)
    (modern-icons-helm-xref-enable))

  (when (member 'lsp modern-icons-enable-packages)
    (require 'modern-icons-lsp)
    (modern-icons-lsp-enable))

  (when (member 'treemacs modern-icons-enable-packages)
    (require 'modern-icons-treemacs)
    (modern-icons-treemacs-enable)))

(provide 'modern-icons)
;;; modern-icons.el ends here
