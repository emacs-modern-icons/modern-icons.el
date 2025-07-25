;;; modern-icons-helm.el --- Modern icons for Helm -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Version: 0.1.0
;; Created: May 27, 2025
;; Homepage: https://github.com/taquangtrung/modern-icons.el
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp, icons, vscode-icons, helm

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
;; (require 'modern-icons-helm)

;; Acknowledgement:
;;
;; This package is inspired by the following packages:
;; - helm-icons: <https://github.com/yyoncho/helm-icons>

;;; Code:

(require 'dash)
(require 'modern-icons)
(require 'helm)
(require 'helm-locate)
(require 'helm-files)
(require 'helm-grep)
(require 'helm-imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General configuration for Helm

(defun modern-icons-helm-file-name-icon (file-name)
  "Get icon by matching exact FILE-NAME."
  (when-let* ((icon (modern-icons-icon-for-file-name file-name)))
    (concat (propertize " " 'display icon) " ")))

(defun modern-icons-helm-file-ext-icon (file-name)
  "Get icon by matching the extension of FILE-NAME."
  (when-let* ((icon (modern-icons-icon-for-file-ext file-name)))
    (concat (propertize " " 'display icon) " ")))

(defun modern-icons-helm-dir-icon (dir-name)
  "Get icon by matching DIR-NAME."
  (when-let* ((icon (or (modern-icons-icon-for-dir dir-name)
                        (modern-icons-default-dir-icon))))
    (concat (propertize " " 'display icon) " ")))

(defun modern-icons-helm-buffer-icon (buffer-name)
  "Get icon for a BUFFER-NAME."
  (let* ((icon (or (modern-icons-icon-for-buffer buffer-name)
                   (modern-icons-icon-for-file buffer-name))))
    (and icon (concat (propertize " " 'display icon) " "))))

(defun modern-icons-helm-mode-icon (mode-name)
  "Get icon for a MODE-NAME."
  (when-let* ((icon (modern-icons-icon-for-mode mode-name)))
    (concat (propertize " " 'display icon) " ")))

(defun modern-icons-helm-workspace-icon (workspace-name)
  "Get icon by matching WORKSPACE-NAME."
  (when-let* ((icon (or (modern-icons-icon-for-workspace workspace-name)
                        (modern-icons-default-file-icon))))
    (concat (propertize " " 'display icon) " ")))

(defun modern-icons-helm-add-icons (candidates source)
  "Add icon to a buffer source or a file source.
CANDIDATES is the list of Helm candidates."
  (let ((source-name (cdr (assoc 'name source))))
    ;; (message "SOURCE NAME: %s" source-name)
    (-map
     (-lambda (candidate)
       (let* ((display (if (listp candidate) (car candidate) candidate))
              (candidate (if (listp candidate) (cdr candidate) candidate))
              (buffer (cond ((bufferp candidate) candidate)
                            ((stringp candidate) (get-buffer candidate))
                            (t nil)))
              (file-name nil)
              (icon (cond
                     ((equal source-name "Git branches")
                      (modern-icons-helm-file-name-icon ".git"))
                     ((equal source-name "find-library")
                      (modern-icons-helm-mode-icon "emacs-lisp-mode"))
                     ((member source-name '("+workspace/switch-to"
                                            "persp-frame-switch"))
                      (modern-icons-helm-workspace-icon candidate))
                     (buffer
                      (with-current-buffer buffer
                        (setq buff-name (buffer-name)
                              file-name (buffer-file-name))
                        (or (and file-name
                                 (not (file-directory-p file-name))
                                 (modern-icons-helm-file-name-icon file-name))
                            (modern-icons-helm-buffer-icon buff-name)
                            (and (not (equal major-mode 'fundamental-mode))
                                 (modern-icons-helm-mode-icon major-mode))
                            (and file-name
                                 (or (and (file-directory-p file-name)
                                          (modern-icons-helm-dir-icon file-name))
                                     (modern-icons-helm-file-ext-icon file-name)))
                            (and (or (char-equal ?* (aref buff-name 0))
                                     (char-equal ?\s (aref buff-name 0)))
                                 (modern-icons-helm-mode-icon 'temporary-mode))
                            (modern-icons-helm-mode-icon 'fundamental-mode))))
                     ((stringp candidate)
                      (setq file-name candidate)
                      ;; Remove quotation in quoted file-name names if any
                      (when (and (string-prefix-p "'" file-name)
                                 (string-suffix-p "'" file-name)
                                 (> (length file-name) 1))
                        (setq file-name (substring file-name 1 (1- (length file-name)))))
                      (or (and (file-directory-p file-name)
                               (modern-icons-helm-dir-icon file-name))
                          (modern-icons-helm-file-name-icon file-name)
                          (modern-icons-helm-file-ext-icon file-name)
                          (modern-icons-helm-mode-icon 'fundamental-mode)))
                     (t (modern-icons-helm-mode-icon 'fundamental-mode)))))
         (cons (concat icon display) candidate)))
     candidates)))

(defun modern-icons-helm-add-transformer (func source)
  "Add func to `filtered-candidate-transformer' slot of helm-source SOURCE."
  (setf (alist-get 'filtered-candidate-transformer source)
        (-uniq (append
                (-let [value (alist-get 'filtered-candidate-transformer source)]
                  (if (seqp value) value (list value)))
                (list func)))))

(defun modern-icons-helm-advisor (func name class &rest args)
  "Advice function for `helm-source' to display icons.
The advised function is `helm-make-source'."
  (let ((result (apply func name class args)))
    ;; (message "DEBUG modern-icons-helm--make: class: %s, name: %s" class name)
    (cond ((member class '(helm-fasd-source
                           helm-ls-git-source
                           helm-ls-git-status-source
                           helm-ls-git-untracked-ignored-source
                           helm-ls-git-untracked-source
                           helm-recentf-source
                           helm-source-buffers
                           helm-source-ffiles
                           helm-source-findutils
                           helm-source-locate
                           helm-source-projectile-buffer))
           (modern-icons-helm-add-transformer #'modern-icons-helm-add-icons result))
          ((-any? (lambda (source-name) (s-match source-name name))
                  '("Buffers in hg project"
                    "Elisp libraries"
                    "Find"
                    "Hg files list"
                    "Hg status"
                    "Last killed files"
                    "Locate"
                    "Projectile directories"
                    "Projectile files in current Dired buffer"
                    "Projectile files"
                    "Projectile projects"
                    "Projectile recent files"
                    "Projectile"
                    "Read File Name"
                    "Recent files and dirs"
                    "Recentf"
                    "Switch to project"
                    "TeX-master-file-ask"
                    "byte-compile-file"
                    "byte-recompile-file"
                    "dired-create-directory"
                    "dired-do-copy"
                    "dired-do-rename"
                    "ediff-buffers"
                    "ediff-files"
                    "helm-find"
                    "kill-buffer"
                    "persp-add-buffer"
                    "persp-remove-buffer"
                    "persp-frame-switch"
                    "project-find-file"
                    "rename-current-buffer"
                    "rename-current-file"
                    "save-current-file"
                    "switch-to-buffer"
                    "+workspace/switch-to"))
           (modern-icons-helm-add-transformer #'modern-icons-helm-add-icons result)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific configuration for `helm-files'

(defun modern-icons-helm-files-advisor (func disp fname &rest args)
  "Advise `helm-files' to display icons. The advised function is
`helm-ff-prefix-filename'."
  (let* ((icon (cond ((null args) nil)
                     ((or (string-match "/\\'" disp)
                          (equal helm-buffer "*helm-mode-dired-create-directory*"))
                      (or (modern-icons-icon-for-dir disp)
                          (modern-icons-default-dir-icon)))
                     (t (or (modern-icons-icon-for-file disp)
                            (modern-icons-default-file-icon)))))
         (disp (if icon (concat (propertize " " 'display icon) " "
                                (propertize disp 'face 'helm-ff-prefix))
                 disp)))
    (apply func disp fname args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific configuration for `helm-grep'

(defun modern-icons-helm-grep-advisor (func &rest args)
  "Advise `helm-grep' to display icons.
The advised function is `helm-grep--filter-candidate-1'."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific configuration for `helm-imenu'

(defun modern-icons-helm-imenu-advisor (_func type &rest _args)
  "Advise `helm-imenu' to display icons.
The advised function is `helm-imenu-icon-for-type'."
  (modern-icons-icon-for-code type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions

;;;###autoload
(defun modern-icons-helm-enable ()
  "Enable `modern-icons-helm'."
  (interactive)
  ;; General configuration for almost all Helm packages.
  (advice-add 'helm-make-source :around #'modern-icons-helm-advisor)
  ;; Configure `helm-locate'
  (modern-icons-helm-add-transformer #'modern-icons-helm-add-icons helm-source-locate)
  ;; Configure `helm-files'
  (advice-add 'helm-ff-prefix-filename :around #'modern-icons-helm-files-advisor)
  ;; Configure `helm-grep'
  (advice-add 'helm-grep--filter-candidate-1 :around #'modern-icons-helm-grep-advisor)
  ;; Configure `helm-imenu'
  (advice-add 'helm-imenu-icon-for-type :around #'modern-icons-helm-imenu-advisor)
  (when (called-interactively-p 'any)
    (message "Modern-icons-helm is enabled!")))

(provide 'modern-icons-helm)
;;; modern-icons-helm.el ends here
