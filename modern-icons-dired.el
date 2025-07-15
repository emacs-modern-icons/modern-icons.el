;;; modern-icons-dired.el --- Modern icons for dired-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ta Quang Trung

;; Author: Ta Quang Trung <taquangtrungvn@gmail.com>
;; Version: 0.1.0
;; Created: April 06, 2025
;; Homepage: https://github.com/taquangtrung/modern-icons.el
;; Package-Requires: ((emacs "28.1"))
;; Keywords: lisp, icons, dired

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
;; (require 'modern-icons-dired)
;; (add-hook 'dired-mode-hook 'modern-icons-dired-mode)

;; Acknowledgement:
;;
;; This package is inspired by the following packages:
;; - all-the-icons-dired: <https://github.com/jtbm37/all-the-icons-dired>
;; - nerd-icons-dired: <https://github.com/rainstormstudio/nerd-icons-dired>

;;; Code:

(require 'dired)
(require 'modern-icons)

(defvar modern-icons-dired-mode)

(defun modern-icons-dired-add-overlay (pos string)
  "Add overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'modern-icons-dired-overlay t)
    (overlay-put ov 'after-string string)))

(defun modern-icons-dired-get-overlays (beg end)
  "Get all modern-icons-dired overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'modern-icons-dired-overlay))
   (overlays-in beg end)))

(defun modern-icons-dired-remove-all-overlays ()
  "Remove all `modern-icons-dired' overlays."
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (modern-icons-dired-get-overlays (point-min) (point-max)))))

(defun modern-icons-dired-display-icons ()
  "Dispay icons of files in a Dired buffer."
  (modern-icons-dired-remove-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (dired-move-to-filename nil)
        (let ((file (dired-get-filename 'relative 'noerror)))
          (when file
            (let* ((icon (cond ((file-directory-p file)
                                (or (modern-icons-icon-for-dir file)
                                    (modern-icons-default-dir-icon)))
                               (t (or (modern-icons-icon-for-file file)
                                      (modern-icons-default-file-icon)))))
                   (icon-str (propertize " " 'display icon))
                   (inhibit-read-only t))
              (modern-icons-dired-add-overlay (dired-move-to-filename)
                                              (concat icon-str "\t"))))))
      (forward-line 1))))

(defun modern-icons-dired-advisor (func &rest args)
  "Advice function for `dired-mode' to display icons.
FUNC is one of the dired functions that display files."
  (let ((result (apply func args)))
    (setq-local tab-width 1)
    (modern-icons-dired-display-icons)
    result))

(defun modern-icons-dired-enable ()
  "Enable `modern-icons-dired'."
  (interactive)
  ;; Enable icons in the current dired buffer.
  (when (derived-mode-p 'dired-mode)
    (setq-local tab-width 1)
    (modern-icons-dired-display-icons))
  ;; Advise other functions.
  (advice-add 'dired-readin :around #'modern-icons-dired-advisor)
  (advice-add 'dired-revert :around #'modern-icons-dired-advisor)
  (advice-add 'dired-internal-do-deletions :around #'modern-icons-dired-advisor)
  (advice-add 'dired-insert-subdir :around #'modern-icons-dired-advisor)
  (advice-add 'dired-create-directory :around #'modern-icons-dired-advisor)
  (advice-add 'dired-do-redisplay :around #'modern-icons-dired-advisor)
  (advice-add 'dired-kill-subdir :around #'modern-icons-dired-advisor)
  (advice-add 'dired-do-kill-lines :around #'modern-icons-dired-advisor)
  (advice-add 'wdired-abort-changes :around #'modern-icons-dired-advisor)
  (when (called-interactively-p 'any)
    (message "Modern-icons-dired is enabled!")))

(defun modern-icons-dired-disable ()
  "Disable `modern-icons-lsp'."
  (interactive)
  ;; Disable icons in the current dired buffer.
  (when (derived-mode-p 'dired-mode)
    (modern-icons-dired-remove-all-overlays))
  ;; Remove advice from dired functions.
  (advice-remove 'dired-readin #'modern-icons-dired-advisor)
  (advice-remove 'dired-revert #'modern-icons-dired-advisor)
  (advice-remove 'dired-internal-do-deletions #'modern-icons-dired-advisor)
  (advice-remove 'dired-insert-subdir #'modern-icons-dired-advisor)
  (advice-remove 'dired-do-kill-lines #'modern-icons-dired-advisor)
  (advice-remove 'dired-create-directory #'modern-icons-dired-advisor)
  (advice-remove 'dired-do-redisplay #'modern-icons-dired-advisor)
  (advice-remove 'dired-kill-subdir #'modern-icons-dired-advisor)
  (advice-remove 'wdired-abort-changes #'modern-icons-dired-advisor)
  (when (called-interactively-p 'any)
    (message "Modern-icons-dired is disabled!")))

(provide 'modern-icons-dired)
;;; modern-icons-dired.el ends here
