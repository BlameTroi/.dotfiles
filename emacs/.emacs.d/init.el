;;; init.el -- Troi's emacs configuration. -*- lexical-binding: t -*-


;;; Commentary:

;; This init was inspired by many things I've seen exploring,
;; I don't remember where all the ideas came from and any list
;; I come up with would be incomplete. Web searches, github,
;; reddit, emacswiki are all starting points.
;;
;; Setting up evil-mode has been a hassle and while my hands are
;; used to some of the common vim commands, I have decided that
;; I'll spend less time just learning base emacs with default
;; keybinds.


;;; Code:


;; Tweak garbage collection during both init and normal
;; processing. Also fire off a garbage collect if the
;; frame goes out of focus.
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
(add-function
 :after after-focus-change-function
 (lambda () (unless (frame-focus-state) (garbage-collect))))


;; Bootstrap straight.el if it is not already here.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package and straight-use-package working together.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;; Define any utility functions for this config file.
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;; General defaults.
(setq user-full-name "Troy Brumley"
      user-mail-address "BlameTroi@gmail.com")
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil)

;; Add my lisp to load-path.
(push "~/.emacs.d/site-lisp/" load-path)


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


;; I like to pick up where I left off.
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)


;; Automatically create missing parent directories when visiting a new
;; file.
(defun troi/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist. Create it?" parent-directory)))
      (make directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'troi/create-non-existent-directory)


;; I need bigger fonts.
(set-frame-font "Hack Nerd Font Mono-15.0" nil t)


;; Minimize typing.
(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode)


;; Bring in the system clipboard.
(setq select-enable-clipboard t)


;; Make the UI quieter, more uniform, and generally to my liking.
(setq-default visible-bell t
              initial-scratch-message nil)
(setq inhibit-startup-screen t
      use-file-dialog nil
      use-dialog-box nil
      read-file-name-completion-ignore-case t)


;; Hilight matching parens.
(setq-default show-paren-delay 0)
(show-paren-mode)


;; I go back and forth on line numbers.
;; (global-display-line-numbers-mode)
;; (setq display-line-numbers-width 4)


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


;; Use the right cl libraries. Do this before loading any
;; packages.
(require 'cl-lib)


;; Load org early to get the current version and not whatever
;; was bundled with emacs.
(use-package org)
(setq org-directory "~/projects/org")


;; Make sure exec path is correct.
;; TODO should a set of baked environment variables be used
;;      instead?
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))


;; External search utilities.
(use-package ag :ensure t)
(use-package fzf :ensure t)


;; ws-butler only cleans up whitespace on lines touched in the edit
;; session.
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))


;; GIT version control.
(use-package magit
  :ensure t)


;; Completion. Is there another option?
;; (use-package company
;;   :ensure t)
;; (add-hook 'after-init-hook 'global-company-mode)


;; Paredit for lisps and general parenthetical goodness in all
;; languages.
;; (use-package paredit
;;   :ensure t)


;; A first application package. A still in development blood glucose
;; tracking application I found on github. I've forked it to better
;; control when I pick up changes.
;;
;; md-arif-shaikh/bgt is the original
(use-package bgt
  :straight (bgt :type git :host github :repo "blametroi/bgt")
  :ensure t
  :config
  (setq bgt-file-name "~/projects/org/bgt.org"
        bgt-csv-file-name "~/projects/org/bgt.csv"
        bgt-python-file "~/.emacs.d/straight/repos/bgt/bgt.py"
        bgt-python-path "python3"))


;; Theming support, though I'm currently using an old color theme.
(use-package green-is-the-new-black-theme :ensure t)
(use-package green-screen-theme :ensure t)
(use-package alect-themes :ensure t)
(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
(set-face-foreground 'font-lock-comment-face "#fc0")


;; parens should be colorful and show matches
(use-package rainbow-delimiters
  :ensure t
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )



;; Make sure zsh uses shell mode.
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))


;; Automatically insert common headers and comments into newly
;; created files.
(auto-insert-mode t)
(setq auto-insert-directory "~/.emacs.d/site-templates")


;; Fortran and F90 tweaks.
(require 'troi-init-fortran)


;; if i ever use paradox again, this is the pattern for storing a
;; token or other 'secret' data. the private directory should not
;; be under source control.
;;
;; The "paradox-token" file is supposed to contain this line:
;;     (setq paradox-github-token "<YOUR_TOKEN>")
;; (load (locate-user-emacs-file "private/paradox-token")) ; :noerror :nomessage)


(provide 'init)

;;; init.el ends here

