;;; init-pasc.el --- major mode for lazarus and free pascal  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Troy Brumley

;; Author: Troy Brumley <BlameTroi@gmail.com>
;; Keywords: languages

;; This file is for use in GNU Emacs.

;;; Commentary:

;; TODO: re-enable after testing

;; ;; User customization for Pascal mode
;; (setq pascal-indent-level       3
;;       pascal-case-indent        2
;;       pascal-auto-newline       nil
;;       pascal-tab-always-indent  t
;;       pascal-auto-endcomments   t
;;       pascal-auto-lineup        '(all)
;;       pascal-type-keywords      '("array" "file" "packed" "char"
;; 				     "integer" "real" "string" "record")
;;       pascal-start-keywords     '("begin" "end" "function" "procedure"
;; 				     "repeat" "until" "while" "read" "readln"
;; 				     "reset" "rewrite" "write" "writeln")
;;       pascal-separator-keywords '("downto" "else" "mod" "div" "then"))

;; TODO defer/autoload
(require 'pasc-mode)

(add-to-list 'auto-mode-alist '("\\.pas\\'" . pasc-mode))

(provide 'init-pasc)

;;; init-pasc.el ends here
