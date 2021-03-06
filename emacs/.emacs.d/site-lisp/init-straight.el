;;; init-straight.el --- set up straight & use-package -*- lexical-binding: t; -*-

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

;; Part of my init.el, split into logical groupings.

;;; Code:


;; Use the right cl libraries. Do this before loading any
;; packages.
(require 'cl-lib)


;; Bootstrap straight.el if it is not already here.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package and straight-use-package working together.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;; bind-key comes with use-package, so may as well invoke it here.
(require 'bind-key)


(provide 'init-straight)
;;; init-straight.el ends here
