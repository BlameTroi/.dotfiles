;;; init-external-tools.el --- packages that support externals such as ag or fzf  -*- lexical-binding: t; -*-

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

;; Just about anything that involves an external executable can go here.
;; Some of the bigger applications/modes (eg., git) rate their own
;; initialization.

;;; Code:

;; External search utilities.
;; TODO: is more setup needed beyond loading?
(use-package ag
  :ensure t)
(use-package rg
  :ensure t)
(use-package fzf
  :ensure t)

(provide 'init-external-tools)
;;; init-external-tools.el ends here
