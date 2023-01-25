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

;; I have older eyes and need bigger fonts.
;; 'default-frame-alist is used to set the font when running emacsclient in gui
;; mode, otherwise the fonts are unreadably small. on a tui, the following
;; machts nichts.

(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font Mono-18.0"))
(set-frame-font "SauceCodePro Nerd Font Mono-18.0" nil t)

;; Theming support with some of my favorites. I spend too much time down this
;; rabbit hole. That said, fewer colors and darker modes work best for my eyes.

;; (use-package green-is-the-new-black-theme :ensure t)
;; (use-package green-screen-theme :ensure t)

(use-package almost-mono-themes
  :ensure t
  :config
  (load-theme 'almost-mono-black t))
  ;; (load-theme 'almost-mono-gray t)
  ;; (load-theme 'almost-mono-cream t)
  ;; (load-theme 'almost-mono-white t))

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
