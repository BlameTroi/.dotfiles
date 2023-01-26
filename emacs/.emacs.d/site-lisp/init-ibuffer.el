;;; init-ibuffer.el --- ibuffer preferences          -*- lexical-binding: t; -*-

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

;; ibuffer is rich enough to warrant its own init file outside of
;; ui-behavior.

;;; Code:

;; ibuffer. from https://cestlaz.github.io/posts/using-emacs-34-ibuffer-emmet/
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("troi"
               ("emacs-config" (filename . ".emacs.d"))
               ("dired" (mode . dired-mode))
               ("org" (or
                       (mode . org-mode)
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
                           (mode . org-mode)
                           (derived-mode . org-mode)
                           (mode . org-agenda-mode)
                           (mode . markdown-mode)
                           (mode . LaTeX-mode)
                           (mode . text-mode)
                           (mode . pdf-view-mode)))
               ))))
;;(get mode 'derived-mode-parent) . prog-mode))))))
;; (or
;;                (mode . pascal-mode)
;;                (mode . eldoc-mode)
;;                (mode . fortran-mode)
;;                (mode . f90-mode)
;;                (mode . gas-mode)
;;                (mode . gdb-mode)
;;                (mode . emacs-lisp-mode)
;;                (mode . slime-mode)
;;                (mode . python-mode)
;;                (mode . c++-mode)))
;;               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "troi")))

;; Don't show these via (add-to-list 'ibuffer-never-show-predicates "zowie")

;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Reduce buffer kill queries.
(setq ibuffer-expert t)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
