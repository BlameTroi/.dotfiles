;;; init-pascal.el --- pascal mode initialization    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Troy Brumley

;; Author: Troy Brumley <BlameTroi@gmail.com>
;; Keywords: lisp, convenience

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

;;; Code:

;; TODO: how to defer?
;; i think this is where i would load a customized version in place
;; of the standard pascal-mode
;; (require 'pascal-mode)
;; (autoload 'pascal-mode "pascal-mode.el")

(setq auto-mode-alist (cons '("\\.pas\\'" . opascal-mode)
                             auto-mode-alist))

;; the default pascal mode seems broken, so i'm checking out the
;; default opascal mode. since i tend to run with $mode objfpc
;; i think this should work ok.

(add-hook 'opascal-mode-hook (function
                             (lambda ()
                               (setq opascal-indent-level          2)
                               (setq opascal-compound-block-indent 2)
                               (setq opascal-case-label-indent     0)
                               (setq opascal-tab-always-indents    t))))

;;                                (setq pascal-auto-newline       nil)
;;                                (setq opascal-tab-always-indent  t)
;;                                (setq pascal-auto-endcomments   t)
;;                                (setq pascal-auto-lineup        '(all))
;;                                (setq pascal-type-keywords      '("array" "file" "packed"
;;                                                                  "char" "integer" "real"
;;                                                                  "string" "record"))
;;                                (setq pascal-start-keywords     '("begin" "end" "function"
;;                                                                  "procedure" "repeat"
;;                                                                  "until" "while" "read"
;;                                                                  "readln" "reset"
;;                                                                  "rewrite" "write"
;;                                                                  "writeln"))
;;                                (setq pascal-separator-keywords '("downto" "else" "mod"
;;                                                                  "div" "then"))
;;                                )))

(provide 'init-pascal)
;;; init-pascal.el ends here
