;;; init-lisp.el --- lisp related settings           -*- lexical-binding: t; -*-

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

;; Set up for lispish programming.

;;; Code:

;; A repl is a good thing.
(use-package geiser
  :ensure t
  :config
  (setq geiser-repl-use-other-window nil))

(use-package geiser-chez
  :ensure t
  :after geiser)

;; txb: do i want chez scheme back?

;; Racket mode
(use-package racket-mode
  :ensure t)

;; Word on the street is that this is the way.
;; txb: paredit and racket have some sort of conflict as of late 2022, be
;; aware and check the racket-mode repo on github if problems arise. there
;; are other options for smart parens if paredit becomes too much of a
;; problem.
(use-package paredit
  :ensure t)

;; Use the pretty print evals:
;; txb: turn off while working through tutorials
;; (global-set-key (kbd "M-:") 'pp-eval-expression)
;; (global-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)

(provide 'init-lisp)
;;; init-lisp.el ends here
