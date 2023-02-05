;;; init-go.el --- go-mode -*- lexical-binding: t; -*-

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

;; Starting with just the base mode, will add more bells and whistles
;; only if needed.

;;; Code:

(use-package go-mode
  :ensure t
  :diminish)

(add-hook 'go-mode-hook
          (lambda ()
            (set-fill-column 0)))
(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'init-go)
;;; init-vim.el ends here
