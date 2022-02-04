;;; package -- doom config.
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Troy Brumley"
      user-mail-address "blametroi@gmail.com")


;; Why not lisp?
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)


;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font "Hack Nerd Font Mono-14.0")


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'wheatgrass)
(setq doom-theme 'alect-black)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/org/")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Completion delay. Set long for now, but for manual set to nil. Value is seconds.
(setq company-idle-delay 1.0)


;; Turn of some parts of lsp-mode, suggestions from hlissner.
(setq lsp-up-sideline-enable nil
      lsp-ui-doc-enable nil
      lspenable-symbol-highlight nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; some more tweaks
(setq-default delete-by-moving-to-trash t
              window-combination-resize t)

(setq undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      make-backup-files t
      password-cache-expiry nil
      scroll-margin 2
      evil-ex-substitute-global t)

(display-time-mode 1)

(global-subword-mode 1)

(toggle-frame-maximized)

;;(setq confirm-kill-emacs t) ;; nil this once i'm done fucking around

;; i shouldn't be using the customize system, but if i do let's
;; segregate it so i notice i did so.
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)


;; try to clean up the doom buffer.
(custom-set-faces!
  '(doom-dashboard-banner :inherit default)
  '(doom-dashboard-loaded :inherit default))
;;(setq spacemacs-theme-comment-bg nil)



;; backups & autosaves


;; zsh
;; (add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
;; (add-hook 'sh-mode-hook
;;           (lambda ()
;;             (if (string-match "\\.zsh$" buffer-file-name)
;;                 (sh-set-sell "zsh"))))


;; the globally ignored directories are not working the way i expect them
;; to from the doc.
;; projectile weirdness, i think it's best to only
;; explicitly mark projects, so let's not autodetect
;; things like .git or package.json.
;;
;; projectile-globally-ignored-files
;; projectile-globally-ignored-directories
;; projectile-globally-ignored-file-suffixes
;; projectile-globally-ignored-modes
(after! projectile
(setq projectile-project-root-files-bottom-up '(".projectile" ".project"))
(setq projectile-globally-ignored-directories (append projectile-globally-ignored-directories '(".git" ".exercism")))
)

(provide 'config)
;;; config.el ends here
