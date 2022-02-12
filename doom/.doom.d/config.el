;;; config.el --- Description -*- lexical-binding: t; -*-

;;; Commentary:

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;;; Code:

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
;;(setq doom-font "Hack Nerd Font Mono-14.0")
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 28)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 36)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 28)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'wheatgrass)
;;(setq doom-theme 'alect-black)
;;(load-theme 'base16-synth-midnight-dark t)
(load-theme 'base16-greenscreen t)
;(setq doom-theme 'manoj-dark)


;; modeline readability with some themes needs help
;; (custom-set-faces!
;;   '(doom-modeline-bar :background "lightgrey")
;;   '(doom-modeline-buffer-file :forground "green")
;;   '(doom-modeline-buffer-modified :foreground "orange"))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/org/")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Completion delay. Set long for now, but for manual set to nil. Value is seconds.
;; (setq company-idle-delay 0.3)


;; Turn of some parts of lsp-mode, suggestions from hlissner.
;; (setq lsp-up-sideline-enable nil
;;       lsp-ui-doc-enable nil
;;       lspenable-symbol-highlight nil)

;; some more tweaks
(setq-default delete-by-moving-to-trash t
              window-combination-resize t)

(setq-default history-length 1000)

(setq undo-limit 8000000
      auto-save-default t
      make-backup-files t
      password-cache-expiry nil
      scroll-margin 2)
(display-time-mode 1)
(global-subword-mode 1)
(toggle-frame-maximized)


;; i shouldn't be using the customize system, but if i do let's
;; segregate it so i notice i did so.
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))


(setq evil-vsplit-window-right t
      evil-split-window-below t)

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
;; (defun projectile-ignored-project-function (filepath)
;;   "Return t if FILEPATH is within any of `projectile-ignored-projects'"
;;   (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
;; (after! projectile
;;   (setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/")
;;         projectile-project-root-files-bottom-up '(".projectile" ".project")
;;         projectile-globally-ignored-directories (append projectile-globally-ignored-directories '(".git" ".exercism"))))


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


;; evil tweaks.
(after! evil
  (setq evil-want-fine-undo t
        evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring


;; company completion tweaks.
(after! company
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))


;; smart parens are different in org-mode.
(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))


;; fortran stuff.
;;(setq flycheck-gfortran-language-standard nil)


;; colors in info.
;; (use-package! info-colors
;;   :commands (info-colors-fontify-node))
;; (add-hook 'Info-selection-hook 'info-colors-fontify-node)


;; theme-magic to spread the theme.
;; (use-package! theme-magic
;;   :commands theme-magic-from-emacs
;;   :config
;;   (defadvice! theme-magic--auto-extract-16-doom-colors ()
;;     :override #'theme-magic--auto-extract-16-colors
;;     (list
;;      (face-attribute 'default :background)
;;      (doom-color 'error)
;;      (doom-color 'success)
;;      (doom-color 'type)
;;      (doom-color 'keywords)
;;      (doom-color 'constants)
;;      (doom-color 'functions)
;;      (face-attribute 'default :foreground)
;;      (face-attribute 'shadow :foreground)
;;      (doom-blend 'base8 'error 0.1)
;;      (doom-blend 'base8 'success 0.1)
;;      (doom-blend 'base8 'type 0.1)
;;      (doom-blend 'base8 'keywords 0.1)
;;      (doom-blend 'base8 'constants 0.1)
;;      (doom-blend 'base8 'functions 0.1)
;;      (face-attribute 'default :foreground))))


(provide 'config)
;;; config.el ends here
