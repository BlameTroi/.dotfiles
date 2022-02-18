;;; init-files-directories.el --- file system locations & behavior  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Troy Brumley

;; Author: Troy Brumley <BlameTroi@gmail.com>
;; Keywords: internal, lisp

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

;; Anything pertaining to the file system as accessed from emacs.

;;; Code:

;; Automatically insert common headers and comments into newly
;; created files.
(auto-insert-mode t)
(setq auto-insert-directory "~/.emacs.d/site-templates")


;; Keep directories clean by stashing autosaves and backups
;; elsewhere.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setq backup-by-copying t)


;; Get the various custom-set-variable blocks out init.el. I make
;; an effort to not use the customize interface at all, but if
;; something creeps in I don't want it cluttering up version
;; control.
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;; Automatically create missing parent directories when visiting a new
;; file.
(defun troi/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist. Create it?" parent-directory)))
      (make directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'troi/create-non-existent-directory)



(provide 'init-files-directories)
;;; init-files-directories.el ends here
