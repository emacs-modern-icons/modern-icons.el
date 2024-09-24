;;; modern-icons-treemacs.el --- Modern icons for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Version: 0.1.0
;; Created: May 24, 2025
;; Homepage: https://github.com/taquangtrung/modern-icons.el
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp, icons, treemacs

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
;;   (require 'modern-icons-treemacs)
;;   (modern-icons-treemacs-enable)

;;; Code:

(require 'treemacs)
(require 'modern-icons)

(defun modern-icons-treemacs-icon (extensions icon-class icon-name)
  "Create a treemacs icon for EXTENSIONS from ICON-CLASS and ICON-NAME."
  (let* ((size 16)
         (icon (modern-icons-create-icon icon-class icon-name size))
         (icon-str (concat (propertize " " 'display icon)
                           (propertize " " 'display '(space :width 0.5)))))
    (treemacs-create-icon :icon icon-str
                          :extensions extensions
                          :fallback 'same-as-icon)))

;; Create `modern-icons' them for treemacs.
(treemacs-create-theme "modern-icons"
  :config
  (progn
    ;; Create icons for file extensions
    (dolist (item (append modern-icons-file-ext-icon-alist
                          modern-icons-file-name-icon-alist))
      (let* ((file-ext (car item))
             (icon-data (cadr item))
             (icon-class (car icon-data))
             (icon-name (cadr icon-data))
             (icon (modern-icons-create-icon icon-class icon-name))
             (icon-str (concat (propertize " " 'display icon)
                               (propertize " " 'display '(space :width 0.5))))
             (fallback " "))
        (let* ((gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
               (tui-icons (treemacs-theme->tui-icons treemacs--current-theme)))
          (ht-set! gui-icons file-ext icon-str)
          (ht-set! tui-icons file-ext fallback))))

    ;; Symbol icons
    (modern-icons-treemacs-icon '(root-closed) "file-icons" "default_root_folder.svg")
    (modern-icons-treemacs-icon '(root-open) "file-icons" "default_root_folder_opened.svg")
    (modern-icons-treemacs-icon '(dir-closed) "file-icons" "default_folder.svg")
    (modern-icons-treemacs-icon '(dir-open) "file-icons" "default_folder_opened.svg")
    (modern-icons-treemacs-icon '(tag-closed) "symbol-icons" "chevron-right.svg")
    (modern-icons-treemacs-icon '(tag-open) "symbol-icons" "chevron-down.svg")
    (modern-icons-treemacs-icon '(tag-leaf) "symbol-icons" "tag.svg")
    (modern-icons-treemacs-icon '(error) "symbol-icons" "error.svg")
    (modern-icons-treemacs-icon '(warning) "symbol-icons" "warning.svg")
    (modern-icons-treemacs-icon '(info) "symbol-icons" "info.svg")
    (modern-icons-treemacs-icon '(mail) "symbol-icons" "mail.svg")
    (modern-icons-treemacs-icon '(bookmark) "symbol-icons" "bookmark.svg")
    (modern-icons-treemacs-icon '(briefcase) "symbol-icons" "briefcase.svg")
    (modern-icons-treemacs-icon '(calendar) "symbol-icons" "calendar.svg")
    (modern-icons-treemacs-icon '(close) "symbol-icons" "close.svg")
    (modern-icons-treemacs-icon '(house) "symbol-icons" "home.svg")
    (modern-icons-treemacs-icon '(list) "symbol-icons" "list-unordered.svg")
    (modern-icons-treemacs-icon '(repeat) "symbol-icons" "sync.svg")
    (modern-icons-treemacs-icon '(screen) "symbol-icons" "vm.svg")
    (modern-icons-treemacs-icon '(suitcase) "symbol-icons" "briefcase.svg")

    ;; Custom dir icons
    (modern-icons-treemacs-icon '("src-closed") "file-icons" "folder_src.svg")
    (modern-icons-treemacs-icon '("src-open") "file-icons" "folder_src_opened.svg")
    (modern-icons-treemacs-icon '("build-closed") "file-icons" "folder_dist.svg")
    (modern-icons-treemacs-icon '("build-open") "file-icons" "folder_dist_opened.svg")
    (modern-icons-treemacs-icon '("test-closed") "file-icons" "folder_test.svg")
    (modern-icons-treemacs-icon '("test-open") "file-icons" "folder_test_opened.svg")
    (modern-icons-treemacs-icon '("bin-closed") "file-icons" "folder_binary.svg")
    (modern-icons-treemacs-icon '("bin-open") "file-icons" "folder_binary_opened.svg")
    (modern-icons-treemacs-icon '("git-closed") "file-icons" "folder_git.svg")
    (modern-icons-treemacs-icon '("git-open") "file-icons" "folder_git_opened.svg")
    (modern-icons-treemacs-icon '("github-closed") "file-icons" "folder_github.svg")
    (modern-icons-treemacs-icon '("github-open") "file-icons" "folder_github_opened.svg")
    (modern-icons-treemacs-icon '("public-closed") "file-icons" "folder_public.svg")
    (modern-icons-treemacs-icon '("public-open") "file-icons" "folder_public_opened.svg")
    (modern-icons-treemacs-icon '("private-closed") "file-icons" "folder_private.svg")
    (modern-icons-treemacs-icon '("private-open") "file-icons" "folder_private_opened.svg")
    (modern-icons-treemacs-icon '("tmp-closed" "temp-closed") "file-icons" "folder_temp.svg")
    (modern-icons-treemacs-icon '("tmp-open" "temp-open") "file-icons" "folder_temp_opened.svg")
    (modern-icons-treemacs-icon '("readme-closed" "docs-closed") "file-icons" "folder_docs.svg")
    (modern-icons-treemacs-icon '("readme-open" "docs-open") "file-icons" "folder_docs_opened.svg")
    (modern-icons-treemacs-icon '("screenshots-closed") "file-icons" "folder_images.svg")
    (modern-icons-treemacs-icon '("screenshots-open") "file-icons" "folder_images_opened.svg")

    ;; Fallback icons
    (modern-icons-treemacs-icon '(fallback) "file-icons" "default_file.svg")))

;;;###autoload
(defun modern-icons-treemacs-enable ()
  "Enable `modern-icons' theme for Treemacs."
  (interactive)
  (treemacs-load-theme "modern-icons")
  (when (called-interactively-p 'any)
    (message "Modern-icons-treemacs is enabled!")))

(provide 'modern-icons-treemacs)
;;; modern-icons-treemacs.el ends here
