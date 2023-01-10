;;; init-ui-behavior.el --- base emacs ui tweaks     -*- lexical-binding: t; -*-

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

;; Some visual and some behavior mostly of base emacs--things that don't
;; require loading a package.

;;; Code:

;; Let's try no menu bar.
;;(menu-bar-mode -1)

;; Simplify the mode line.
(use-package diminish
  :ensure t)


;; I like to pick up where I left off.
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)


;; Minimize typing.
(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode)


;; System clipboard joins the kill ring.
(setq select-enable-clipboard t)


;; Allow pixelwise resizing.
(setq frame-resize-pixelwise t)


;; Make the UI quieter, more uniform, and generally to my liking.
(setq-default visible-bell t
              initial-scratch-message nil)
(setq inhibit-startup-screen t
      use-file-dialog nil
      use-dialog-box nil
      read-file-name-completion-ignore-case t)


;; Most people expect delete to actually delete an active highlighted
;; region these days.
(delete-selection-mode 1)


;; Hilight matching parens.
(setq-default show-paren-delay 0)
(show-paren-mode)


;; I go back and forth on line numbers.
(global-display-line-numbers-mode)
(setq display-line-numbers-width 6)


;; But I do like column numbers, and I like them one based.
(column-number-mode)
(setq mode-line-position-column-format " C%C")


;; Whitespace and other global formatting. I removed the display
;; of trailing whitespace because it provides little benefit. Using
;; ws-butler cleans up anything I add without changing other code.
;; TODO wrap/truncate?
(setq-default fill-column 70
              indent-tabs-mode nil
              tab-width 4)
(setq sentence-end-double-space nil)


;; ws-butler only cleans up whitespace on lines touched in the edit
;; session.
(use-package ws-butler
  :ensure t
  :diminish
  :config
  (ws-butler-global-mode))


;; which-key is very helpful!
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))


;; Usually a newline should indent in a programming mode.
(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))


;; An understore is a word character in many prpogramming languages.
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(diminish 'eldoc-mode)


;; Hilight todo's, this package also supports moving from tag to tag
;; but I haven't wired the keybinds yet.
(use-package hl-todo
  :ensure t
  :diminish
  :config
  (global-hl-todo-mode))


;; Easier window navigation.
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(provide 'init-ui-behavior)
;;; init-ui-behavior.el ends here
