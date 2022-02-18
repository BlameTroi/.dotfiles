;;; init-bgt.el --- blood glucose tracking in org    -*- lexical-binding: t; -*-

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

;;
;; A still in development blood glucose tracking application I
;; found on github. I've forked it to better control when I pick
;; up changes.
;;
;; The original is md-arif-shaikh/bgt is on github.

;;; Code:


(use-package bgt
  :straight (bgt :type git :host github :repo "blametroi/bgt")
  :ensure t
  :config
  (setq bgt-file-name "~/projects/org/bgt.org"
        bgt-csv-file-name "~/projects/org/bgt.csv"
        bgt-python-file "~/.emacs.d/straight/repos/bgt/bgt.py"
        bgt-python-path "python3"))


(provide 'init-bgt)
;;; init-bgt.el ends here
