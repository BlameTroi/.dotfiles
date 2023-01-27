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

;; Utility functions:
(require 'init-troi-functions)

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
(require 'init-ibuffer)
(require 'init-git)

;; NOTE: it is reported that yas doesn't play nice with org
;; (require 'init-yasnippet)

;; TODO: 'init-completion ... company or ???
;; Completion. Is there another option?
;; (use-package company
;;   :ensure t)
;; (add-hook 'after-init-hook 'global-company-mode)

;; TODO: textual/documentation mode stuff

;; Language/mode specific initialization.
(require 'init-auto-mode)
(require 'init-lisp)
(require 'init-pascal)

;; if i ever use paradox again, this is the pattern for storing a
;; token or other 'secret' data. the private directory should not
;; be under source control.
;;
;; The "paradox-token" file is supposed to contain this line:
;;     (setq paradox-github-token "<YOUR_TOKEN>")
;; (load (locate-user-emacs-file "private/paradox-token")) ; :noerror :nomessage)

(provide 'init)

;;; init.el ends here

