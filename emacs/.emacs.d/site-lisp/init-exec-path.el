;;; init-exec-path.el --- make sure exec path is updated from shell  -*- lexical-binding: t; -*-

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

;; On OS X the execution path is often wrong. On my Linux systems, I want
;; to pick up the default venv for Python code.

;;; Code:


(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


(provide 'init-exec-path)
;;; init-exec-path.el ends here
