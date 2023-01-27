;;; init-troi-functions.el --- any defuns for my init  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Troy Brumley

;; Author: Troy Brumley <BlameTroi@gmail.com>
;; Keywords: lisp, convenience, local

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

;; Various helper functions for general initialization.

;;; Code:

(defun troi/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(provide 'init-troi-functions)
;;; init-troi-functions.el ends here
