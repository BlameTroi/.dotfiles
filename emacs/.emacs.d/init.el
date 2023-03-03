;;; init.el -- Troi's emacs configuration. -*- lexical-binding: t -*-

;; Author: Troy Brumley <blametroi@gmail.com>

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

;; February 2023
;;
;; I've been doing a lot of configuration as I push into learning
;; emacs, emacs lisp, lisp, org, and other things. This config
;; really needs to become literate using org/babel but first I am
;; going to undo what turned out to be a bad idea.
;;
;; I thought splitting configs into separate files and calling
;; them in the init would be a good approach. It might have been
;; when I had a better memory than I do now, but buffer switching
;; and file hunting take too much of my short term memory.
;;
;; First, pull everything back into one (hopefully well laid out)
;; ginormous init and then consider literizing it.

;;; Code:

;; Help debug config errors. Lord knows I make them!
(setq debug-on-error t)
(setq debug-on-quit t)
(setq message-log-max t) ;; we don't want to lose any startup log info

;; Add my lisp to load-path.
(push "~/.emacs.d/site-lisp/" load-path)

;; General defaults.
(setq user-full-name "Troy Brumley"
      user-mail-address "BlameTroi@gmail.com")
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil)

;; Garbage collection.
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

;; Use the proper cl libraries. Do it early!
(require 'cl-lib)


;; Some helper functions.
(defun troi/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;; Package management is via straight.el. It must be bootstrapped and
;; enabled before we start loading any other packages.
;;
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


;; Simplify the mode line!
(use-package diminish
  :ensure t)


;; bind-key comes with use-package, so may as well invoke it here.
(require 'bind-key)


;; On OS X the execution path is often wrong. On my Linux systems, I want
;; to pick up the default venv for Python code.
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; org-mode should be loaded via external packaging early in our init
;; to avoid bringing in the default version, which is likely stale.
(use-package org
  :ensure t
  :diminish
  :config
  (setq org-directory "~/org")
  (setq org-log-done 'time)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  )


;; Automatically insert common headers and comments into newly created
;; files.
(auto-insert-mode t)
(setq auto-insert-directory "~/.emacs.d/site-templates")

;; Keep working directories clean by stashing autosaves and backups
;; elsewhere.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setq backup-by-copying t)
;; TODO: I'm seeing some dangling files ".#blah..." when editing in some
;; directories ... why? It must be for autosave or rollback.


;; Automatically create missing parent directories when visiting a new
;; file.
(defun troi/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist. Create it?" parent-directory)))
      (make directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'troi/create-non-existent-directory)


;; Fonts, Faces, and Themes. On a clear day you can see in color.
;;
;; I have older eyes and need bigger fonts.
;;
;; 'default-frame-alist is used to set the font when running emacsclient in gui
;; mode, otherwise the fonts are unreadably small. on a tui, the following
;; machts nichts.
;; (setq troi/default-font "Brutalist Mono-18.0")
(add-to-list 'default-frame-alist '(font . "Brutalist Mono-16.0"))
(set-frame-font "Brutalist Mono-16.0")
;; (add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font Mono-18.0"))
;; (set-frame-font "SauceCodePro Nerd Font Mono-18.0" nil t)


;; Theming support with some of my favorites. I spend too much time down this
;; rabbit hole. That said, fewer colors and darker modes work best for my eyes.
;;
;; As one comment put it on a vim theme, I don't want my screen to look like a
;; clown puked on it.
;;
;; almost-mono-black is my current favorite, but green-is-the-new-black-theme
;; and green-screen-theme are pretty good. Some of the alect- themes have
;; promise.
;;(use-package almost-mono-themes
  ;;:ensure t)
  ;;:config
  ;;  (load-theme 'almost-mono-black t))
  ;; (load-theme 'almost-mono-gray t))
  ;;(load-theme 'almost-mono-cream t))
  ;; (load-theme 'almost-mono-white t))


;; So far this is the most workable theme. Getting diff highlighting
;; back to something I could actually see took a bit of digging, and
;; this feels like a hack, but it isn't clear how to change the
;; custom-theme-set-faces after the theme loads. I can live with
;; this solution and I've left some comments on ohter faces I might
;; want to tweak.
(use-package plan9-theme
             :ensure t
             :after diff-hl
             :config
             (load-theme 'plan9 t)
             (set-face-background 'diff-hl-insert "green")
             (set-face-background 'diff-hl-delete "red")
             (set-face-background 'diff-hl-change "DarkOrange")
             )
;;
;; Theme Faces
;; (custom-theme-set-faces
;;  'plan9
;; ;;;; diff
;;  `(diff-added ((,class (:foreground ,fg :background ,green-light))
;;                (t (:foreground ,fg :background ,green-light))))
;;  `(diff-changed ((t (:foreground ,yellow))))
;;  `(diff-context ((t (:foreground ,fg))))
;;  `(diff-removed ((,class (:foreground ,fg :background ,red-light))
;;                  (t (:foreground ,fg :background ,red-light))))
;;  `(diff-refine-added ((t :inherit diff-added :background ,green-light :weight bold)))
;;  `(diff-refine-change ((t :inherit diff-changed :weight bold)))
;;  `(diff-refine-removed ((t :inherit diff-removed :background ,red-light :weight bold)))
;;  `(diff-header ((,class (:foreground ,fg :weight bold))
;;                 (t (:foreground ,purple-light :weight bold))))
;;  `(diff-file-header
;;    ((,class (:foreground ,fg :background ,cyan-light :weight bold))
;;     (t (:foreground ,fg :background ,cyan-light :weight bold))))
;;  `(diff-hunk-header
;;    ((,class (:foreground ,green :weight bold))
;;     (t (:foreground ,green :weight bold))))
;; ;;;; diff-hl
;;  `(diff-hl-insert ((t (:foreground ,fg :background ,green))))
;;  `(diff-hl-delete ((t (:foreground ,fg :background ,red))))
;;  `(diff-hl-change ((t (:foreground ,fg :background ,yellow))))
;; ;;;;; git-gutter
;;  `(git-gutter:added ((t (:foreground ,green :weight bold))))
;;  `(git-gutter:deleted ((t (:foreground ,red :weight bold))))
;;  `(git-gutter:modified ((t (:foreground ,yellow :weight bold))))
;;  `(git-gutter:unchanged ((t (:foreground ,fg :weight bold))))
;; ;;;;; git-gutter-fr
;;  `(git-gutter-fr:added ((t (:foreground ,green  :weight bold))))
;;  `(git-gutter-fr:deleted ((t (:foreground ,red :weight bold))))
;;  `(git-gutter-fr:modified ((t (:foreground ,yellow :weight bold))))
;;  )


;; I'd like to use this but it's going to
;; require more font tweaking for variable-pitch-mode
;; so I'm holding off for now.
;; (use-package poet-theme
;;   :ensure t)
;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (variable-pitch-mode 1)))


;; UI/UX tweaks and customization.


;; I like to pick up where I left off.
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)


;; Minimize typing.
(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode)


;; System clipboard joins the kill ring.
;; NOTE: this doesn't work under kitty terminal, but i'm ok with that.
(setq select-enable-clipboard t)


;; Allow pixelwise resizing.
(setq frame-resize-pixelwise t)


;; Put some snark and useful snippets in scratch.
(setq initial-scratch-message
    ";; - 'Tis but a scratch!
;; - A scratch? Your arm's off!
;; - No, it isn't!

;; Start guile repl:
(geiser-guile)

;; To shutdown an emacs server:
(save-buffers-kill-emacs)")


;; Make the UI quieter, more uniform, and generally to my liking.
(setq-default visible-bell t)
(setq inhibit-startup-screen t
      use-file-dialog nil
      use-dialog-box nil
      read-file-name-completion-ignore-case t)


;; I found mention of making emacs scroll a bit more like vim. I
;; prefer that so we'll try this:
(setq scroll-step 1
      scroll-conservatively 10000)


;; Most people expect delete to actually delete an active highlighted
;; region these days.
(delete-selection-mode 1)


;; Parens should be colorful and show matches.
(setq-default show-paren-delay 0)
(show-paren-mode)
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
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; I go back and forth on line numbers, but I do like column numbers,
;; and I like them one based.
(global-display-line-numbers-mode)
(setq display-line-numbers-width 4)
(column-number-mode)
(setq mode-line-position-column-format " C%C")


;; Whitespace and other global formatting. I removed the display
;; of trailing whitespace because it provides little benefit. Using
;; ws-butler cleans up anything I add without changing other code.
(setq-default ;; fill-column 0
              indent-tabs-mode nil
              tab-width 2)
(setq sentence-end-double-space nil)


;; Use visual-fill-column with visual-line-mode to properly "break"
;; long lines on wide displays. It's 2023, all displays are wide.
;; (global-visual-line-mode)
(use-package visual-fill-column
  :ensure t
  :diminish
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))


;; Let's try adding a visual hint about the fill column, I find
;; the reminder helpful in certain modes. These settings seem to
;; work ok on the gui, must test in tui.  Yep, works in tui.
;;
;; Toggle with display-fill-column-indicator-mode. All the variables
;; become buffer local when set. I could setq-default but I'm not
;; doing so at this time. Leaving here for reference.
;;
;; display-fill-column-indicator        't
;; display-fill-column-indicator-column 60   ;; t for fill-column, integer to override
;; display-fill-column-character        32   ;; ascii space, if i may show my age

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
  (which-key-setup-side-window-right-bottom))


;; An understore is a word character in many prpogramming languages.
;; TODO: is there a prog-mode syntax table, and should this be set
;; there as well?
(modify-syntax-entry ?_ "w" (standard-syntax-table))


;; Hilight todo and other tags.
(use-package hl-todo
  :ensure t
  :diminish
  :config
  (global-hl-todo-mode)
  (add-to-list 'hl-todo-keyword-faces '("TXB" . "#cc9393"))
  (add-to-list 'hl-todo-keyword-faces '("txb" . "#cc9393"))
  (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert))
;; NOTE: following are from a reddit post
;; (use-package hl-todo
;;        :ensure t
;;        :custom-face
;;        (hl-todo ((t (:inherit hl-todo :italic t))))
;;        :hook ((prog-mode . hl-todo-mode)
;;               (yaml-mode . hl-todo-mode)))


;; Easier navigation between windows.
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; Switch up some help and key discovery. Found this on stack.
(use-package discover-my-major
  :ensure t
  :bind
  (("C-h C-m" . discover-my-major)
   ("C-h M-m" . discover-my-mode))
  )


;; Set up ibuffer, provide my own filter groups.
;; from https://cestlaz.github.io/posts/using-emacs-34-ibuffer-emmet/
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("troi"
               ("emacs-config" (filename . ".emacs.d"))
               ("dired" (mode . dired-mode))
               ("org" (or
                       (mode . org-mode)
                       (derived-mode . org-mode)
                       (mode . org-agenda-mode)
                       (name . "^.*org$")))
               ("magit" (name . "\*magit"))
               ("help" (or
                        (name . "\*Help\*")
                        (name . "\*Apropose\*")
                        (name . "\*info\*")))
               ("emacs" (or
                         (name . "\*straight-*")
                         (name . "^\\*Pp Eval Output\\*$")
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("shell" (or
                         (mode . eshell-mode)
                         (mode . shell-mode)))
               ("lisp/scheme" (or
                               (name . "\*geiser*")
                               (name . "\* Chez REPL*")
                               (mode . emacs-lisp-mode)
                               (mode . scheme-mode)
                               (mode . lisp-mode)
                               (mode . racket-mode)))
               ("programming" (or
                               ;; have to check for pascal separately because it isn't
                               ;; implemented as a derived mode yet.
                               ;; and also oberon
                               (mode . pascal-mode)
                               (mode . oberon-mode)
                               (derived-mode . prog-mode)))
               ("Writing" (or
                           (mode . markdown-mode)
                           (mode . gfm-mode)
                           (mode . LaTeX-mode)
                           (mode . text-mode)
                           (mode . pdf-view-mode)))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (set-fill-column 0) ;; TODO: might be able to ditch this based on other settings
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "troi")))
;; TODO: Are there buffers I don't want to show? See ibuffer-never-show-predicates!

;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Reduce buffer kill queries.
(setq ibuffer-expert t)


;; External search utilities.
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package fzf
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg --no-heading -nH"
        ;; fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 10))


;; While researching paredit I tripped over ivy/counsel/swipe
;; and the combination looks straight forward and useful with
;; minimal configuration. Helm is anything but minimal from
;; what I've read.
;;
;; The following is from https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
;; and then I hacked around on it to get counsel and swiper to work as well.
;;
;; All of ivy/counsel/swiper are in one github repo, aboabo/swiper. I can't tell
;; for sure if I need to use-package all three or not.
(use-package swiper
  :ensure t)
(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))
(use-package ivy :ensure t
  ;; :diminish (ivy-mode . "")
  ;;:bind
  ;;(:map ivy-mode-map
  ;;      ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)                 ;; add ‘recentf-mode’ and bookmarks
  (setq ivy-height 5)                              ;; number of result lines to display
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp) ;; for swiper
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )
(use-package ivy-rich
  :ensure t
  :after ivy)
(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1))


;; GIT version control. git-gutter wasn't displaying so I've
;; reverted to diff-hl.
(use-package magit
  :ensure t)
(use-package diff-hl
  :after magit
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
;; (use-package git-gutter :ensure t
;;   :after magit
;;   :config
;;   ;; If you enable global minor mode
;;   (global-git-gutter-mode t)
;;   ;; If you would like to use git-gutter.el and linum-mode
;;   (git-gutter:linum-setup)
;;   ;; (global-set-key (kbd "C-x C-g") 'git-gutter)
;;   ;; (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;;   ;; ;; Jump to next/previous hunk
;;   ;; (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
;;   ;; (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
;;   ;; ;; Stage current hunk
;;   ;; (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;;   ;; ;; Revert current hunk
;;   ;; (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
;;   ;; ;; Mark current hunk
;;   ;; (global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)
;;   )


;; Additional documentation, not sure I'll keep this.
(use-package devdocs
  :ensure t
  ;; I don't think there's much need for configuration, beyond maybe
  ;; a keybind.
  ;; (global-set-key (kbd "C-h D") 'devdocs-lookup)
  ;; Not using yet.
  )


;; Projectile ... hopefully not projectile vomiting! Several tools
;; I am looking at want it, so here we go but hopefully we'll keep
;; it simple.
;; (use-package projectile
;;   :ensure t
;;   :diminish)
;; (projectile-mode +1)
;; ;; Recommended keymap prefix on Windows/Linux
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; Trying out auto-complete since it is used in a tutorial I am working
;; through. Other alternatives include company.
(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)) ;; no diminish yet


;; Markdown mode.
;;
;; there are two modes, default markdown-mode and gfm-mode for
;; github flavored markdown. So much for standardization.
;;
;; use-package has a :mode option where the auto-mode-alist is updated
;; modified. I don't think I need to use that here.
;;
;; (autoload 'markdown-mode "markdown-mode"
;;    "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist
;;              '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
;; (autoload 'gfm-mode "markdown-mode"
;;    "Major mode for editing GitHub Flavored Markdown files" t)
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "cmark"))


;; Read epub with nov.el. There's an xwidget's version of this, but
;; it doesn't run well and I don't feel like rebuilding emacs just
;; for that.
;;
;; There are many customizations possible for font, filling, and
;; justification. See https://depp.brause.cc/nov.el/ if the
;; default isn't getting it done.
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))


;; I sometimes need to look at vimscript.
(use-package vimrc-mode
  :ensure t
  :diminish
  :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))


;; A repl is a good thing. I'm using guile for scheming. And also some
;; Racket.
(use-package geiser
  :ensure t
;;  :diminish
  :config
  (setq geiser-repl-use-other-window nil))
(use-package geiser-guile
  :ensure t
;;  :diminish
  :after geiser)

;; Auto-complete for geiser, see jeka.frama.io tutorial on geiser.
(use-package ac-geiser
  :ensure t
  :after (geiser-guile auto-complete)
  :config
  (add-hook 'geiser-mode-hook 'ac-geiser-setup)
  (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
  (add-to-list 'ac-modes 'geiser-repl-mode))


;;(use-package racket-mode
;;  :ensure t
;;  :diminish)
;; let's not at this point
;; (use-package geiser-racket
;;   :ensure t
;;   :diminish
;;   :after geiser)


;; Word on the street is that this is the way.
;; txb: paredit and racket have some sort of conflict as of late 2022, be
;; aware and check the racket-mode repo on github if problems arise. there
;; are other options for smart parens if paredit becomes too much of a
;; problem.
;;
;; I'm having issues keyboarding with paredit. It's me, not the mode. I need
;; to try something else.
;;
;; Alternatives are paxedit and smartparens. Paxedit requires that paredit
;; is installed but does not need it enabled.
;;
;; Configuration for all of these are more hooks and more keybinds than I'm
;; ready to deal with right now.
;;
;; (use-package paredit
;;   :ensure t
;;   :config
;;   (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;   (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;   (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;;   ;; Eldoc setup. Added while working through a geiser tutorial
;;   ;; but don't know that it's stricktly needed, but it may help
;;   ;; with paredit. I'm not sure if this should be separate from
;;   ;; the paredit config.
;;   (require 'eldoc) ; if not already loaded
;;   (eldoc-add-command
;;    'paredit-backward-delete
;;    'paredit-close-round)
;;   )


;; Use the pretty print evals:
;; txb: turn off while working through tutorials
;; (global-set-key (kbd "M-:") 'pp-eval-expression)
;; (global-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)


;; Standard ML.
(use-package sml-mode
  :ensure t
  :diminish
  :mode ("\\.sml\\'" . sml-mode))


;; Go go language ... edit!
;;
;; Trying to keep this minimal. I've found gopls setup information but
;; I don't think I need a full lsp. Working from the go-mode author's
;; site at https://honnef.co/articles/writing-go-in-emacs/ and the
;; emacs wiki I'll build up what I need/want.
(use-package go-mode
  :ensure t
  :diminish
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  ;; (add-hook 'go-mode-hook (lambda ()
  ;;                           (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  ;; (add-hook 'go-mode-hook (lambda ()
  ;;                           (local-set-key (kbd "C-c i") 'go-goto-imports)))
  )

;; a full lsp may not be required for my usage.
;; And here come some bells and whistles for langauges. LSP mode set up
;; for golang. UI support commands are here but clipped out for now.
;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init (setq lsp-keymap-prefix "C-c l")
;;   :config (lsp-enable-which-key-integration t)
;;   :hook ((go-mode) . lsp))
;;
;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config (setq lsp-ui-doc-enable t))


;; This whole emacs excursion actually started with Pascal. It's still
;; my favorite compiled language.
;;
;; I've found the standard pascal.el to be pretty broken, and the few
;; alternatives poorly documented and while somewhat less broken still
;; not to my liking.
;;
;; Nobody indents right!
;;
;; (setq pascal-indent-level       2
;;       pascal-case-indent        2
;;       pascal-auto-newline       nil
;;       pascal-indent-nested-functions nil
;;       pascal-tab-always-indent  nil
;;       pascal-auto-endcomments   nil
;;       pascal-auto-lineup        '(all) ;; paramlist, declaration, case
;;       pascal-type-keywords      '("array" "file" "packed" "char"
;; 	    		                  "integer" "real" "string" "record")
;;       pascal-start-keywords     '("begin" "end" "function" "procedure"
;;         		                  "repeat" "until" "while" "read" "readln"
;; 	     		                  "reset" "rewrite" "write" "writeln")
;;       pascal-separator-keywords '("downto" "else" "mod" "div" "then"))


;; if i ever use paradox again, this is the pattern for storing a
;; token or other 'secret' data. the private directory should not
;; be under source control.
;;
;; The "paradox-token" file is supposed to contain this line:
;;     (setq paradox-github-token "<YOUR_TOKEN>")
;; (load (locate-user-emacs-file "private/paradox-token")) ; :noerror :nomessage)


;; General mode related options that don't have a proper home elsewhere.
;; Make sure zsh uses shell mode.
(troi/add-auto-mode 'sh-mode "\\.zsh\\'")


;; Diminishing that wasn't done elsewhere.
(diminish 'eldoc-mode)
(diminish 'visual-line-mode)
(diminish 'auto-revert-mode)
(diminish 'counsel-mode)
(diminish 'ivy-mode)


;; Display tweaking for various modes ...
;; There has to be a better way to do this, but I haven't found it yet.
(add-hook 'text-mode-hook
          (lambda ()
            (set-fill-column 75)
            (visual-line-mode)))
;; (add-hook 'help-mode-hook
;;           (lambda ()
;;             (set-fill-column 75)
;;             (visual-line-mode)))
;; Usually a newline should indent in a programming mode.
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)
            ))
;;             (setq fill-column 0)
;;             (setq truncate-lines t)))
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (setq fill-column 0)
;;             (setq truncate-lines t)))
;;(add-hook 'prog-mode-hook
;;          (lambda ()
;;            (set-fill-column 0)
;;            (truncate-lines)))


;; Get the various custom-set-variable blocks out init.el. I make a
;; concentrated effort to NEVER use the customize interface, but if
;; something creeps in I don't want it cluttering up my init in
;; version control.
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;; Turn off configuration debug.
(setq debug-on-error nil)
(setq debug-on-quit nil)


(provide 'init)

;;; init.el ends here
