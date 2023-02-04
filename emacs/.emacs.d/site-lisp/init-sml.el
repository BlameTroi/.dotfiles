;;; init-vim.el --- sml-mode                 -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Troy Brumley

;; Author: Troy Brumley <BlameTroi@gmail.com>
;; Keywords: languages, tools

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

;; I sometimes need to look at vimscript ...

;;; Code:

(use-package sml-mode
  :ensure t
  :diminish
  :mode ("\\.sml\\'" . sml-mode))

(add-hook 'sml-mode-hook
          (lambda ()
            (set-fill-column 80)))

(require 'sml-mode)
;;(add-to-list 'auto-mode-alist
;;             '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(provide 'init-sml)
;;; init-vim.el ends here
