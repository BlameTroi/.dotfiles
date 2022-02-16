;; Key maps.
(mapc #'define-prefix-command
      '(evil-application-map
        evil-buffer-map
        evil-error-map
        evil-file-map
        evil-git-map
        evil-help-map
        evil-global-leader-map
        evil-insert-map
        evil-jump-map
        evil-option-map
        evil-search-map
        evil-tab-map))


(use-package magit
  :ensure t

  :bind ( :map evil-git-map
          ("cc" . magit-clone)
          ("fl" . magit-log-buffer-file)
          ("fm" . magit-file-dispatch)
          ("fF" . magit-find-file)
          ("m" . magit-dispatch)
          ("s" . magit-status) ) )

;; Evil mode.
;; Additional packages to install:
;;        evil-easymotion
;;        evil-paredit
;;        evil-search-highlight-persist
;;        evil-visualstar
;;        goto-last-change
(use-package evil
  :defer nil

  :ensure t

  :init
  (setq evil-want-integration t) ;; this should be the default
  (setq evil-want-keybinding nil)
  :bind
  ( :map evil-motion-state-map
    ("SPC" . evil-global-leader-map)
    ("M-SPC" . evil-global-leader-map)
    :map evil-insert-state-map
    ("M_SPC" . evil-global-leader-map)
    :map evil-global-leader-map
    ("a" . evil-application-map)
    ("b" . evil-buffer-map)
    ("f" . evil-file-map)
    ("g" . evil-git-map)
    ("h" . evil-help-map)
    ("i" . evil-insert-map)
    ("j" . evil-jump-map)
    ("o" . evil-option-map)
    ("s" . evil-search-map)
    ("u" . universal-argument)
    ("w" . evil-window-map)
    :map evil-window-map
    ("o" . window-toggle-other-windows)
    :map evil-buffer-map
    ("b" . list-buffers)
    ("B" . bury-buffer)
    ("d" . kill-this-buffer)
    ("e" . eval-buffer)
    ("h" . switch-to-help-buffer)
    ("m" . view-echo-area-messages)
    ("s" . switch-to-scratch-buffer)
    ("x" . kill-buffer-and-window)
    :map evil-file-map
    ("a" . write-file)
    ("c" . copy-file)
    ("f" . find-file)
    ("i" . insert-file)
    ("n" . fileloop-continue)
    ("r" . recover-this-file)
    ("R" . rename-file-and-buffer)
    ("s" . save-buffer)
    :map evil-jump-map
    ("f" . find-function)
    ("v" . find-variable)
    :map evil-option-map
    ("f" . display-fill-column-indicator-mode)
    ("s" . window-toggle-side-windows)
    ("l" . toggle-truncate-lines)
    ("n" . display-line-numbers-mode)
    ("d" . toggle-debug-on-error)
    :map evil-replace-state-map
    ("M-SPC" . evil-global-leader-map)
    :map evil-search-map
    ("c" . evil-ex-nohighlight)
    ("i" . imenu-list-smart-toggle) )

  :config

  (evil-mode 1)
  (setq evil-search-wrap t)
  (setq evil-regexp-search t)


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


  (use-package evil-easymotion
    :ensure t
    :config
    (evilem-default-keybindings "SPC"))

  )

(use-package evil-collection
  :ensure t
  :after evil
  :custom (evil-collection-key-blacklist '("SPC"))
  :config
  (evil-collection-init)
  (evil-collection-buff-menu-setup)
  (evil-collection-unimpaired-setup))
