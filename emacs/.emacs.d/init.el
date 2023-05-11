;;;
;;; init.el, combining Keith Waclena's from his book Use GNU Emacs with my
;;; own preferences. Starting small, my init.el has gone through many changes
;;; and this is an opportunity to intelligently determine what I'll use and
;;; need instead of just "that looks nice" loading.
;;;
;;; Issues and areas for thought:
;;;
;;; KW is using elpa/melpa/gnu where I had pulled in straight.el.
;;;
;;; I'm starting to use Guix (packaging, not the full distro) and I don't
;;; yet know how that will work going forward.
;;;
;;; My programming is likely to be scheme centric with some forays back to
;;; Pascal, Cobol, Fortran, and BAL if I start using Hercules again. I am
;;; doing some exercisms and will sometimes diddle with other languages
;;; but Scheme and Pascal are the top two.

;; recommended minimal starting Emacs Init File
;; extracted from the book:
;; /Use GNU Emacs: The Plain Text Computing Environment/
;; by Keith Waclena
;; https://www.lib.uchicago.edu/keith/emacs/
;;
(require 'package)


(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(when (version< emacs-version "26.3")
  ;; older emacsen < 26.3 may need this
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(with-eval-after-load 'package
  (dolist (arc '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                 ("melpa-stable" . "https://stable.melpa.org/packages/")
                 ("melpa" . "https://melpa.org/packages/")))
    (add-to-list 'package-archives arc t))
  (setq package-archive-priorities
        '(("gnu" . 10) ("nongnu" . 9) ("melpa-stable" . 8) ("melpa" . 7))))

;; thanks to an anonymous EmacsWiki coder
(defun undo-yank (arg)
  "Undo the yank you just did.  Really, adjust just-yanked text
like \\[yank-pop] does, but in the opposite direction."
  (interactive "p")
  (yank-pop (- arg)))
(global-set-key (kbd "C-M-Y") 'undo-yank)

(setq enable-recursive-minibuffers t)

(with-eval-after-load 'chistory
  (setq list-command-history-max 120)
  (define-key command-history-map (kbd "<return>") 'command-history-repeat))

(setq completion-styles '(partial-completion substring flex))
(unless (package-installed-p 'vertico)
  (with-demoted-errors "%s"
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'vertico)))
(with-demoted-errors "%s" (vertico-mode +1))

(unless (package-installed-p 'marginalia)
  (with-demoted-errors "%s"
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'marginalia)))
(with-demoted-errors "%s" (marginalia-mode +1))

(unless (package-installed-p 'windmove)
  (with-demoted-errors "%s"
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'windmove)))
;; <S-{left,right,up,down}> switches windows
(with-demoted-errors "%s" (windmove-default-keybindings))

(winner-mode 1) ; undo window config changes
;; add more felicitous bindings
(define-key winner-mode-map [(control c) (control left)] 'winner-undo)
(define-key winner-mode-map [(control c) (control right)] 'winner-redo)

(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)

(global-set-key (kbd "C-<") 'scroll-left)
(global-set-key (kbd "C->") 'scroll-right)

(setq large-file-warning-threshold (* 100 1024 1024)) ; 100MB

(when (version<= "27.1" emacs-version)  ; only available recently...
  (global-so-long-mode +1))    ; speed up long lines

(setq view-read-only t)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(add-hook 'pdf-view-mode-hook 'auto-revert-mode)

(add-hook 'dired-load-hook (lambda () (require 'dired-x)))

(setq dired-dwim-target t)  ; suggest other visible dired buffer

(unless (package-installed-p 'wgrep)
  (with-demoted-errors "%s"
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'wgrep)))

(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)

;; [sic] <<initialize-package-archives>>

(desktop-save-mode 1)   ; restore files from previous session

(save-place-mode 1)   ; come back to where we were in that file

(global-set-key (kbd "C-+") 'text-scale-adjust) ; embiggen font

(setq-default indent-tabs-mode nil)      ; don't insert tabs

(setq async-shell-command-buffer 'new-buffer) ;multiple async commands ok!
(setq async-shell-command-display-buffer nil) ;don't pop up the buffer

(setq comint-buffer-maximum-size 65336) ; must be able to cat War and Peace!
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

;; goto-address-mode is handy in these modes
(dolist (hook '(shell-mode-hook eshell-mode-hook))
  (add-hook hook #'goto-address-mode))
(add-hook 'prog-mode-hook #'goto-address-prog-mode)

(setq calendar-mark-holidays-flag t        ; colorize holidays in the calendar
      calendar-mark-diary-entries-flag t)  ; also diary entries

(setq org-agenda-include-diary t)  ; incorporate the diary into the agenda

(appt-activate +1)        ; appointment notifications, please
(require 'notifications)  ; also via desktop notifications

;; don't use a separate Frame for the control panel
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; horizontal split is more readable
(setq ediff-split-window-function 'split-window-horizontally)

;; restore window config upon quitting ediff
(defvar ue-ediff-window-config nil "Window config before ediffing.")
(add-hook 'ediff-before-setup-hook
          (lambda ()
            (setq ue-ediff-window-config (current-window-configuration))))
(dolist (hook '(ediff-suspend-hook ediff-quit-hook))
  (add-hook hook
            (lambda ()
              (set-window-configuration ue-ediff-window-config))))

;;;
;;; additions from my init.el that I know I will want.
;;;

;; Keep working directories clean by stashing autosaves and backups
;; elsewhere.
(make-directory "~/.tmp/emacs/auto-save/" t)
(make-directory "~/.tmp/emacs-backup/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" nil)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setq backup-by-copying t)
;; TODO: I'm seeing some dangling files ".#blah..." when editing in some
;; directories ... why? It must be for autosave or rollback.
;; txb: it appears that may be caused by the use of t as the third argument to
;; auto-save-file-name-transforms ... that requests uniquifiation. changing to
;; nil for testing.

;; Minimize typing.
(fset 'yes-or-no-p 'y-or-n-p)

;; Delete when highlighted as in other apps.
(delete-selection-mode 1)

;; I want the menu but not the tool ...
(tool-bar-mode -1)

;; I like column numbers starting from one.
(column-number-mode)
(setq mode-line-position-column-format " C%C")

;; I still need which-key. I am planning to manage most of my installed packages
;; with guix. This approach will quietly ignore it if it isn't installed.
(if (package-installed-p 'which-key)
    (progn
     (require 'which-key)
     (which-key-mode)
     (which-key-setup-side-window-right-bottom)))

;; I found mention of making emacs scroll a bit more like vim. I
;; prefer that so we'll try this:
(setq scroll-step 1
      scroll-conservatively 10000)

;; Put some snark and useful snippets in scratch.
(setq initial-scratch-message
    ";; - 'Tis but a scratch!
;; - A scratch? Your arm's off!
;; - No, it isn't!


;; To shutdown an emacs server:
(save-buffers-kill-emacs)

;; When you wnat Lispy on or off:
(lispy-mode)

")


;; General defaults.
(setq user-full-name "Troy Brumley"
      user-mail-address "BlameTroi@gmail.com")
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil)

;; General mode related options that don't have a proper home elsewhere.
;; Make sure zsh uses shell mode.
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; alternate font "FuraMono Nerd Font"

;; Keep working directories clean by stashing autosaves and backups
;; elsewhere.
(make-directory "~/.tmp/emacs/auto-save/" t)
(make-directory "~/.tmp/emacs-backup/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" nil)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setq backup-by-copying t)

;; Get the various custom-set-variable blocks out init.el. I make a
;; concentrated effort to NEVER use the customize interface, but if
;; something creeps in I don't want it cluttering up my init in
;; version control.
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
