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


;; Add my lisp to load-path.
(push "~/.emacs.d/site-lisp/" load-path)


;; General defaults.
(setq user-full-name "Troy Brumley"
      user-mail-address "BlameTroi@gmail.com")
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil)


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


;; Define any utility functions for this config file.
(defun troi/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;; Initialization is factored out by somewhat logical groupings. Package
;; management via straight.el must come before we start loading any other
;; packages. org-mode should be loaded via external packaging early to
;; avoid bringing in the default version, which is likely stale.


;; infrastructure
(require 'init-straight)
(require 'init-exec-path)
(require 'init-org)
(require 'init-files-directories)
(require 'init-fonts-themes)
(require 'init-ui-behavior)
(require 'init-external-tools)
(require 'init-ido)
(require 'init-auto-mode)
;; todo: 'init-yasnippet


;; more textually specific
;; todo: 'init-completion ... company or ???
;; todo: textual/documentation mode stuff


;; more programming specific
;; todo: 'init-lisp ... paredit, support for scheme, cl
;; todo: 'init-pascal
;; todo: 'init-go
(require 'troi-init-f90)


;; applications
(require 'init-bgt)





;; Completion. Is there another option?
;; (use-package company
;;   :ensure t)
;; (add-hook 'after-init-hook 'global-company-mode)


;; Paredit for lisps and general parenthetical goodness in all
;; languages.
;; (use-package paredit
;;   :ensure t)


;; if i ever use paradox again, this is the pattern for storing a
;; token or other 'secret' data. the private directory should not
;; be under source control.
;;
;; The "paradox-token" file is supposed to contain this line:
;;     (setq paradox-github-token "<YOUR_TOKEN>")
;; (load (locate-user-emacs-file "private/paradox-token")) ; :noerror :nomessage)


(provide 'init)

;;; init.el ends here

