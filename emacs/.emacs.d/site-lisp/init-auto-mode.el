;;; init-auto-mode.el --- hooks such as auto mode    -*- lexical-binding: t; -*-

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

;; General mode related options that don't have a proper home elsewhere.

;;; Code:


;; Make sure zsh uses shell mode.
(troi/add-auto-mode 'sh-mode "\\.zsh\\'")
;;(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))


(provide 'init-auto-mode)
;;; init-auto-mode.el ends here
