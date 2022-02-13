;;; init.el -- Troi's emacs configuration. -*- lexical-binding: t -*-


;;; Commentary:
;; This init was inspired by many things I've seen exploring,
;; I don't remember where all the ideas came from and any list
;; I come up with would be incomplete. Web searching and github
;; are my sources.


;;; Code:


;; Tweak garbage collection during init.el processing.
(setq gc-cons-threshold 50000000)

;; Tweak garbage collection during normal processing and
;; also fire off a garbage collect if the frame goes out
;; of focus.
(setq gc-cons-threshold 2000000)
(add-function
 :after after-focus-change-function
 (lambda () (unless (frame-focus-state) (garbage-collect))))



;; Bootstrap straight.el if it is not already here.
;; Install straight.el
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


;; Some standard libraries.
(use-package cl-lib)     ;; use the right cl libraries
(use-package seq)        ;; sequence operations


;; Load org early to get the current version and not whatever
;; was bundled with emacs.
(use-package org)
(setq org-directory "~/projects/org")


;; Keep directories clean by stashing autosaves and backups
;; elsewhere.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setq backup-by-copying t)


;; encoding, utf-8 everywhere
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)


;; Base packages.
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package green-is-the-new-black-theme :ensure t)
(use-package green-screen-theme :ensure t)
(use-package alect-themes :ensure t)

(use-package ag :ensure t)
(use-package fzf :ensure t)

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(use-package magit
  :ensure t)


;; Evil mode.
;; Additional packages to install:
;;        evil-easymotion
;;        evil-paredit
;;        evil-search-highlight-persist
;;        evil-visualstar
;;        goto-last-change
(use-package evil
  :ensure t

  :init
  (setq evil-want-integration t) ;; this should be the default
  (setq evil-want-keybinding nil)

  :config

  (evil-mode 1)
  (setq evil-search-wrap t)
  (setq evil-regexp-search t)

  (use-package evil-collection
    :ensure t
    :after evil
    :config
    (evil-collection-init))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "bs" 'save-buffer
      "bk" (lambda ()
             (interactive)
             (kill-buffer (current-buffer)))
      "ws" 'evil-window-split
      "wv" 'evil-window-vsplit
      "wc" 'evil-window-delete
      ;; "cs" 'eval-last-sexp
      ;; "q"  'evil-quit
      "he" 'view-echo-area-messages
      ;; "hf" 'counsel-describe-function
      ;; "hv" 'counsel-describe-variable
      "h." 'describe-symbol
      ;; "sb" 'swiper
      ;; "sd" 'counsel-rg
      ))

  (use-package evil-org
    :ensure t
    :config
    (evil-org-set-key-theme
     '(textobjects insert navigation additional shift todo heading))
    (add-hook 'org-mode-hook (lambda () (evil-org-mode))))

  (use-package powerline-evil
    :ensure t
    :config
    (powerline-evil-vim-color-theme))

  )


;; theming and ui
(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
(set-face-foreground 'font-lock-comment-face "#fc0")

;; I need bigger fonts.
(set-frame-font "Hack Nerd Font Mono-15.0" nil t)

;; Avoid but don't completely hide the GUI. Be quiet. Minimize typing.
(column-number-mode)
(display-time-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default
 visible-bell t
 initial-scratch-message nil
 select-enable-clipboard t)  ; this should integrate kill ring with system
(setq inhibit-startup-screen t
      use-file-dialog nil
      use-dialog-box nil
      read-file-name-completion-ignore-case t)
(recentf-mode)


;; Whitespace and other global formatting.
(setq-default
 fill-column 70            ; width for auto line wrap
 indent-tabs-mode nil      ; space it out
 tab-width 4               ; readable
 show-trailing-whitespace t)
(setq sentence-end-double-space nil)


;; parens should be colorful and show matches
;(require 'rainbow-delimiters)
;(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
;(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
;(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
;(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
;(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
;(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
;(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
;(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
;(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
(setq-default show-paren-delay 0)
(show-paren-mode)


;; zsh is my preference
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))


;; Tweak Fortran and f90 modes. Modified from a gist by aradi.

;; Fortran settings
(setq-default fortran-continuation-string "&")
(setq-default fortran-do-indent 4)
(setq-default fortran-if-indent 4)
(setq-default fortran-structure-indent 4)

;; Fortran 90 settings
(setq-default f90-do-indent 4)
(setq-default f90-if-indent 4)
(setq-default f90-type-indent 4)
(setq-default f90-program-indent 4)
(setq-default f90-continuation-indent 6)
(setq-default f90-smart-end 'blink)

;; Swap Return and C-j in Fortran 90 mode
;; not sure i'll like this, but we'll see
(add-hook 'f90-mode-hook
          '(lambda ()
             (define-key f90-mode-map [return] 'f90-indent-new-line)
             (define-key f90-mode-map "\C-j" 'newline)
             (setq fill-column 100)
             (abbrev-mode)
             (setq-default indent-tabs-mode nil)
             (setq whitespace-line-column 100)
             (setq whitespace-style '(face tabs lines-tail empty))
             (whitespace-mode)
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)
             )
          )

;; Read in handy abbreviations for Fortran
(quietly-read-abbrev-file (locate-user-emacs-file "site-lisp/abbrev-f90.el"))


;; get the various custom-set-variable blocks out
;; of my init.el
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;; if i ever use paradox again, this is the pattern for storing a
;; token or other 'secret' data. the private directory should not
;; be under source control.
;;
;; The "paradox-token" file is supposed to contain this line:
;;     (setq paradox-github-token "<YOUR_TOKEN>")
;; (load (locate-user-emacs-file "private/paradox-token")) ; :noerror :nomessage)


(provide 'init)

;;; init.el ends here

