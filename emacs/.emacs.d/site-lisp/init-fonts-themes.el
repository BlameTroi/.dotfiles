;;; init-fonts-themes.el --- fonts, faces, themes    -*- lexical-binding: t; -*-

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

;; On a clear day you can see in color.

;;; Code:


;; I need bigger fonts.
;;(set-frame-font "Hack Nerd Font Mono-15.0" nil t)
;;(set-frame-font "Liberation Mono-14.0" nil t)
(set-frame-font "SouceCodePro Nerd Font Mono-20.0" nil t)
;; Theming support, though I'm currently using an old color theme.
(use-package green-is-the-new-black-theme :ensure t)
(use-package green-screen-theme :ensure t)
(use-package alect-themes :ensure t)
(use-package solarized-theme :ensure t)
;;(use-package gruber-darker-theme :ensure t)
(use-package gruvbox-theme :ensure t)
;; (load-theme 'wombat)
;; (set-face-background 'default "#111")
;; (set-face-background 'cursor "#c96")
;; (set-face-background 'isearch "#c60")
;; (set-face-foreground 'isearch "#eee")
;; (set-face-background 'lazy-highlight "#960")
;; (set-face-foreground 'lazy-highlight "#ccc")
;; (set-face-foreground 'font-lock-comment-face "#fc0")
;; (load-theme 'gruber-darker-theme t)
(load-theme 'gruvbox-dark-hard t)
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


(provide 'init-fonts-themes)
;;; init-fonts-themes.el ends here
